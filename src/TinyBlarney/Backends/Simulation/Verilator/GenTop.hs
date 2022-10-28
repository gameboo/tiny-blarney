{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |

Module      : TinyBlarney.Backends.Simulation.Verilator.GenTop
Description : TinyBlarney's verilator simulation C++ toplevel generation
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Verilator.GenTop (
  genVerilatorTop
) where

import TinyBlarney.Core
import TinyBlarney.Misc.PrettyHelpers.C

import Prelude hiding ((<>))
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Text.PrettyPrint

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Verilator.GenTop: " ++ m

-- | Generate a C++ verilator simulation toplevel
genVerilatorTop :: Circuit -> String
genVerilatorTop c = renderStyle style $ vcat [ headersDoc, genMain c ]
  where
    style = Style { mode = PageMode
                  , lineLength = 80
                  , ribbonsPerLine = 1.2 }
    headersDoc = vcat
      [ text "#include" <+> text "<verilated.h>"
      , text "#include" <+> doubleQuotes (text "simulator_interface_helpers.h")
      , text "#include" <+> doubleQuotes (text $ "V" ++ c.name ++ ".h") ]

-- | Derive the code for the main function of the simulation toplevel
genMain :: Circuit -> Doc
genMain c =
  cFunDef "int" "main" [("int", "argc"), ("char**", "argv")]
          [ cFunCall (text "Verilated::commandArgs")
                     (text <$> ["argc", "argv"]) <> semi
          , cDef ("V" ++ c.name ++ "*", "top") (Just . cNew $ "V" ++ c.name)
          , vcat insCreate
          , vcat outsCreate
          , cWhile (char '!' <> cFunCall (text "Verilated::gotFinish") [])
                   mainLoop
          , vcat outsDestroy
          , vcat insDestroy
          , cFunCall (cIndirectAccess "top" "final") [] <> semi
          , cDelete "top"
          , cReturn (int 0) ]
  where
    mainLoop =
      [ cDef ("bool", "needEval") (Just $ text "false")
      , vcat (assignInPort <$> bitChannelIns)
      , cIf [ ( text "needEval"
              , [ cFunCall (cIndirectAccess "top" "eval") [] <> semi
                , vcat (drainOutPort <$> bitChannelOuts)
                , text ("static int n = 0; printf(\" >>>>> %d <<<<< \\n\", n++);")
                ] ) ]
            Nothing ]
    -- Communication with outside processes
    bitChannels = bitChannel <$> ifcPorts
    bitChannelIns = [ (w, x, y, z) | (In, w, x, y, z) <- bitChannels ]
    bitChannelOuts = [ (w, x, y, z) | (Out, w, x, y, z) <- bitChannels ]
    (insW, insCreate, insUse, insDestroy) = unzip4 bitChannelIns
    (_, outsCreate, outsUse, outsDestroy) = unzip4 bitChannelOuts
    --
    assignInPort (w, _, consume, _) =
      cOrAccum "needEval"
               (hang (sep [consume, text "=="]) 2
                     (text "DIVCEIL" <> parens (int w <> comma <+> int 8)))
      <> semi
    drainOutPort (_, _, produce, _) = produce <> semi
    -- Extract interface port information
    ifcPrefix = "ifc"
    ifcnames = interfaceNames (Just ifcPrefix) c.interface
    ifcPort ctxt = case ctxt.ifc of
      Port pDir w -> (fromMaybe fail $ M.lookup ctxt.path ifcnames, pDir, w)
      _ -> fail
      where fail = err $ "Could not identify interface port, ctxt: "
                         ++ show ctxt ++ " - ifcnames: " ++ show ifcnames
    ifcPorts = onCircuitInterfaceLeaves ifcPort c.interface

-- | Derive bit channel code blocks
bitChannel :: (String, PortDir, BitWidth)
           -> (PortDir, BitWidth, CStmt, CStmt, CStmt)
bitChannel (nm, pDir, w) = (pDir, w, createChannel, useChannel, destroyChannel)
  where
    chanSymStr = ("bit_channel_" ++)
    chanSym = text . chanSymStr
    ffName = nm ++ "_" ++ show w ++ case pDir of In -> "bit_sink"
                                                 Out -> "bit_source"
    createChannel = cDef (chanSymStr "t*", nm) (Just channelInit)
    channelInit = cFunCall (chanSym case pDir of In -> "create_consumer"
                                                 Out -> "create_producer")
                           [doubleQuotes (text ffName), int w]
    useChannel = cFunCall (chanSym $ case pDir of In -> "consume_non_block"
                                                  Out -> "produce")
                          [ text nm
                          , text "(uint8_t*)"
                            <+> char '&' <> parens (cIndirectAccess "top" nm) ]
    destroyChannel = cFunCall (chanSym "destroy") [text nm] <> semi
