{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |

Module      : TinyBlarney.Backends.Simulation.Verilator
Description : TinyBlarney's verilator simulation
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Verilator (
  buildSimulatorWithVerilator
) where

import TinyBlarney.Core
import TinyBlarney.Backends.PrettyHelpers.C
import TinyBlarney.Backends.CodeGeneration.Verilog
import TinyBlarney.Backends.Simulation.SimulatorTypes

import Prelude hiding ((<>))
import Data.Map
import Data.Time
import Control.Monad
import System.IO
import System.Exit
import System.Process
import System.Directory
import Text.PrettyPrint

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Verilator: " ++ m

-- | Build a 'Simulator' for a given 'Circuit' using verilator by first
--   generating the verilog code and the appropriate C++ wrapper to then invoke
--   verilator
buildSimulatorWithVerilator :: Circuit -> IO Simulator
buildSimulatorWithVerilator c = do
  tmp <- getTemporaryDirectory
  now <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "_%H_%M_%s" now
  let tmpDir = tmp ++ "/verilator_" ++ c.name ++ timeStr
  createDirectoryIfMissing True tmpDir
  putStrLn $ "created " ++ tmpDir
  withCurrentDirectory tmpDir do
    -- Generate all the verilog for the given circuit
    putStrLn $ "- Verilog generation for " ++ c.name
    let vs = toList . generateVerilog $ c
    forM_ vs \(modName, modCode) -> writeFile (modName ++ ".v") modCode
    putStrLn $ "  generated verilog for " ++ show (fst <$> vs)
    -- Generate the verilator toplevel
    putStrLn $ "- Verilator toplevel C++ generation for " ++ c.name
    writeFile (c.name ++ ".cpp") (genVerilatorTop c)
    putStrLn $ "  generated C++ for " ++ c.name
    -- Run verilator
    putStrLn $ "- Running verilator"
    verilatorStdout <- openFile ("verilator_stdout") AppendMode
    verilatorStderr <- openFile ("verilator_stderr") AppendMode
    let verilatorProc = (proc "verilator" [ "-cc", c.name ++ ".v"
                                          , "-exe", c.name ++ ".cpp"
                                          , "-o", c.name
                                          , "-Wno-UNSIGNED"
                                          , "-y", "."
                                          , "--x-assign", "unique"
                                          , "--x-initial", "unique" ]) {
            std_out = UseHandle verilatorStdout
          , std_err = UseHandle verilatorStderr
          }
    (_, _, _, verilatorHandle) <- createProcess verilatorProc
    exitCode <- waitForProcess verilatorHandle
    case exitCode of
      ExitFailure n -> err $ "verilator exit status: " ++ show n
                             ++ "\nsee log for more information"
      ExitSuccess -> putStrLn "  verilator ran successfully"
    -- Build the verilator simulator
    putStrLn $ "- Building simulator executable"
    makeStdout <- openFile ("make_stdout") AppendMode
    makeStderr <- openFile ("make_stderr") AppendMode
    let makeProc = (proc "make" [ "-C", "obj_dir", "-j"
                                , "-f", "V" ++ c.name ++ ".mk", c.name ]) {
            std_out = UseHandle makeStdout
          , std_err = UseHandle makeStderr
          }
    (_, _, _, makeHandle) <- createProcess makeProc
    exitCode <- waitForProcess makeHandle
    case exitCode of
      ExitFailure n -> err $ "make exit status: " ++ show n
                             ++ "\nsee log for more information"
      ExitSuccess -> putStrLn "  make ran successfully"
    simPath <- canonicalizePath $ "obj_dir/" ++ c.name
    putStrLn $ "  simulator generated: " ++ simPath
  return $ err ("TODO")

genVerilatorTop :: Circuit -> String
genVerilatorTop c = render $ vcat [ headers, defs, mainDef ]
  where
    headers = vcat
      [ text "#include" <+> text "<verilated.h>"
      , text "#include" <+> doubleQuotes (text $ "V" ++ c.name ++ ".h") ]
    defs = vcat
      [ cDef ("V" ++ c.name ++ "*", "top") Nothing
      , cDef ("vluint64_t", "main_time") (Just $ int 0)
      , text "// Called by $time in Verilog"
      , cFunDef "double" "sc_time_stamp" [] [cReturn $ text "main_time"] ]
    mainDef = cFunDef "int" "main" [("int", "argc"), ("char**", "argv")] body
    body = [ cFunCall (text "Verilated::commandArgs")
                      (text <$> ["argc", "argv"]) <> semi
           , cAssign (text "top") (cNew $ "V" ++ c.name) <> semi
           , cWhile (char '!' <> cFunCall (text "Verilated::gotFinish") [])
                    [ cAssign (cIndirectAccess "top" "clock") (int 0) <> semi
                    , cFunCall (cIndirectAccess "top" "eval") [] <> semi
                    , cAssign (cIndirectAccess "top" "clock") (int 1) <> semi
                    , cFunCall (cIndirectAccess "top" "eval") [] <> semi
                    , cPostIncr "main_time" 1 <> semi ]
           , cFunCall (cIndirectAccess "top" "final") [] <> semi
           , cDelete "top"
           , cReturn (int 0) ]
