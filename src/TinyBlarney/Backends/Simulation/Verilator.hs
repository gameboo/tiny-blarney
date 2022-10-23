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
import TinyBlarney.Backends.CodeGeneration.Verilog
import TinyBlarney.Backends.Simulation.SimulatorTypes
import TinyBlarney.Backends.Simulation.Verilator.GenTop

import qualified Data.Map as M
import Data.Time
import Control.Monad
import System.IO
import System.Exit
import System.Process
import System.FilePath
import System.Directory
import System.Environment

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Verilator: " ++ m

-- | A wrapper around createProcess / waitForProcess capturing stdout/stderr
captureProcess :: FilePath -> [String] -> IO ()
captureProcess cmd args = do
  let stdoutPath = takeBaseName cmd ++ "_stdout"
  let stderrPath = takeBaseName cmd ++ "_stderr"
  pStdout <- openFile stdoutPath AppendMode
  pStderr <- openFile stderrPath AppendMode
  let pCreate = (proc cmd args) { std_out = UseHandle pStdout
                                , std_err = UseHandle pStderr }
  (_, _, _, pHandle) <- createProcess pCreate
  exitCode <- waitForProcess pHandle
  case exitCode of
    ExitFailure n -> err $ cmd ++ " exit status: " ++ show n
                           ++ "\nsee " ++ stdoutPath ++ " or " ++ stderrPath
    ExitSuccess -> putStrLn $ cmd ++ " ran successfully"

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
    let vs = M.toList . generateVerilog $ c
    forM_ vs \(modName, modCode) -> writeFile (modName ++ ".v") modCode
    putStrLn $ "  generated verilog for " ++ show (fst <$> vs)
    -- Generate the verilator toplevel
    putStrLn $ "- Verilator toplevel C++ generation for " ++ c.name
    writeFile (c.name ++ ".cpp") (genVerilatorTop c)
    putStrLn $ "  generated C++ for " ++ c.name
    -- Run verilator
    putStrLn $ "- Running verilator"
    let ifcHelp = "simulator_interface_helpers"
    mBlarneyRoot <- lookupEnv "BLARNEY_ROOT"
    let blarneyRoot = case mBlarneyRoot of
          Just x -> x
          _ -> err $ "environment variable BLARNEY_ROOT must be correctly set"
    let resourceDir = blarneyRoot </> "src/backends-resources/Verilator"
    copyFile (resourceDir ++ "/" ++ ifcHelp ++ ".h")
             (tmpDir ++ "/" ++ ifcHelp ++ ".h")
    copyFile (resourceDir ++ "/" ++ ifcHelp ++ ".c")
             (tmpDir ++ "/" ++ ifcHelp ++ ".c")
    captureProcess "verilator" [ "-cc", c.name ++ ".v"
                               , "-exe"
                               , c.name ++ ".cpp"
                               , ifcHelp ++ ".c"
                               , "-o", c.name
                               , "-Wno-UNSIGNED"
                               , "-y", "."
                               , "--x-assign", "unique"
                               , "--x-initial", "unique" ]
    -- Build the verilator simulator
    putStrLn $ "- Building simulator executable"
    captureProcess "make" [ "-C", "obj_dir", "-j"
                          , "-f", "V" ++ c.name ++ ".mk", c.name ]
    simPath <- canonicalizePath $ "obj_dir/" ++ c.name
    putStrLn $ "  simulator generated: " ++ simPath
  return $ err ("TODO")
