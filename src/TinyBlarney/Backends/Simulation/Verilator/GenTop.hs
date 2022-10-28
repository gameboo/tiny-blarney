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
import TinyBlarney.Misc.Misc
import TinyBlarney.Misc.PrettyHelpers.C
import TinyBlarney.Backends.Simulation.Verilator.Communication

import Prelude hiding ((<>))
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Text.PrettyPrint

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Verilator.GenTop: " ++ m

type Port = (String, BitWidth)
type InPort = Port
type OutPort = Port

-- Extract interface port information
extractIfc :: Circuit -> String -> ([InPort], [OutPort])
extractIfc c pfx = ( [(nm, w) | (In, nm, w) <- ifcPorts]
                   , [(nm, w) | (Out, nm, w) <- ifcPorts] )
  where
    ifcnames = interfaceNames (Just pfx) c.interface
    ifcPort ctxt = case ctxt.ifc of
      Port pDir w -> (pDir, fromMaybe fail $ M.lookup ctxt.path ifcnames, w)
      _ -> fail
      where fail = err $ "Could not identify interface port, ctxt: "
                         ++ show ctxt ++ " - ifcnames: " ++ show ifcnames
    ifcPorts = onCircuitInterfaceLeaves ifcPort c.interface

defTopInputAssigns :: CType -> [InPort] -> Doc
defTopInputAssigns topT inPorts =
  cFunDef "void" "inputAssigns" [(topT++"*", "top"), ("const uint8_t*", "raw")]
          [snd $ foldl assignInPort (0, empty) inPorts]
  where
    assignInPort (off, doc) (nm, w) =
      ( off + w
      , doc $+$ cFunCall (text "bitmemcpy")
                         [ text $ "(void*) &(top->" ++ nm ++ ")", int 0
                         , text "raw", int off, int w ] <> semi )

-- | Derive the code for the main function of the simulation toplevel
defMain :: CType -> [InPort] -> [OutPort] -> Doc
defMain topT inPorts outPorts =
  cFunDef "int" "main" [("int", "argc"), ("char**", "argv")]
          [ cFunCall (text "Verilated::commandArgs")
                     (text <$> ["argc", "argv"]) <> semi
          , cDef (topT ++ "*", "top") (Just . cNew $ topT)
          , cDef ("void*", "simReq") (Just $ cFunCall (text "malloc")
                                                      [int inReqByteW])
          , createInChannel
          , createOutChannel
          , cDef ("bool", "done") (Just $ text "false")
          , cWhile (cNot . cOrs $ [ cFunCall (text "Verilated::gotFinish") []
                                  , text "done" ])
                   [ deqInChannel "simReq"
                   , cSwitch (cDeref $ cCast "uint8_t*" (text "simReq"))
                             [ (int $ fromIntegral Evaluate, handleEvalCmd)
                             , (int $ fromIntegral Finish, handleFinishCmd) ]
                             (Just handleUnknownCmd)
                   ]
          , destroyInChannel
          , destroyOutChannel
          , cFunCall (text "free") [text "simReq"] <> semi
          , cFunCall (cIndirectAccess "top" "final") [] <> semi
          , cDelete "top"
          , cReturn (int 0) ]
  where
    -- channel widths
    inSigsW = sum . (snd  <$>) $ inPorts
    inSigsByteW = ceilDiv inSigsW 8
    inReqW = 8 + 64 + inSigsW
    inReqByteW = 1 + 8 + inSigsByteW
    outSigsW = sum . (snd  <$>) $ outPorts
    outSigsByteW = ceilDiv outSigsW 8
    outRspW = 8 + 64 + outSigsW
    outRspByteW = 1 + 8 + outSigsByteW
    -- channel handles
    (createInChannel, deqInChannel, destroyInChannel) =
      bitChannel In inReqW "simReqSink" "simReqSource"
    (createOutChannel, enqOutChannel, destroyOutChannel) =
      bitChannel Out outRspW "simRspSource" "simRspSink"
    -- event handles
    handleEvalCmd = [text "//todo"]
    handleFinishCmd = [text "//todo"]
    handleUnknownCmd = [text "//todo", text "return 0;"]

-- | Derive bit channel code blocks
bitChannel :: PortDir -> BitWidth -> String -> CIdent
           -> (CStmt, CIdent -> CStmt, CStmt)
bitChannel pDir w ffnm nm = (createChannel, useChannel, destroyChannel)
  where
    chanSymStr = ("bit_channel_" ++)
    chanSym = text . chanSymStr
    createChannel = cDef (chanSymStr "t*", nm) (Just channelInit)
    channelInit = cFunCall (chanSym case pDir of In -> "create_consumer"
                                                 Out -> "create_producer")
                           [doubleQuotes (text ffnm), int w]
    useChannel arg = cFunCall (chanSym $ case pDir of In -> "consume"
                                                      Out -> "produce")
                              [ text nm
                              , cCast "uint8_t*"
                                      (char '&' <> parens (text arg)) ] <> semi
    destroyChannel = cFunCall (chanSym "destroy") [text nm] <> semi

-- | Generate a C++ verilator simulation toplevel
genVerilatorTop :: Circuit -> String
genVerilatorTop c = renderStyle style $ vcat [ headersDoc
                                             , defTopInputAssigns topT inPorts
                                             , defMain topT inPorts outPorts ]
  where
    style = Style { mode = PageMode
                  , lineLength = 80
                  , ribbonsPerLine = 1.2 }
    headersDoc = vcat
      [ text "#include" <+> text "<verilated.h>"
      , text "#include" <+> doubleQuotes (text "simulator_interface_helpers.h")
      , text "#include" <+> doubleQuotes (text $ "V" ++ c.name ++ ".h") ]
    (inPorts, outPorts) = extractIfc c "ifc"
    topT = "V" ++ c.name
