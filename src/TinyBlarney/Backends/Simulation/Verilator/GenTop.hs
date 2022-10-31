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

-- Local helper types to identify circuit ports
-- | A 'Port' is a port name and its width '(String, BitWidth)'
type Port = (String, BitWidth)
-- | A 'Port' representing a circuit input
type InPort = Port
-- | A 'Port' representing a circuit output
type OutPort = Port

-- | Extract the 'Port's from a 'Circuit' interface
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

-- | Define the function called to assign the circuit input ports from a buffer
--   of a simulation "evaluate" request
defTopInputAssigns :: CType -> [InPort] -> Doc
defTopInputAssigns topT inPorts =
  cFunDef "void" "inputAssigns"
          [(topT ++ "*", "top"), ("const uint8_t*", "raw")]
          [snd $ foldl assignInPort (0, empty) inPorts]
  where
    assignInPort (off, doc) (nm, w) | (byteOff, bitOff) <- quotRem off 8 =
      ( off + w
      , doc $+$ cFunCall (text "bitmemcpy")
                         [ cCast "void*" (text $ "&(top->" ++ nm ++ ")")
                         , int 0
                         , cCast "void*" (text "raw +" <+> int byteOff)
                         , int bitOff
                         , int w ] <> semi )

-- | Define the function called to assign the buffer of a simulation "evaluated"
--  response from the circuit output ports
defTopOutputAssigns :: CType -> [OutPort] -> Doc
defTopOutputAssigns topT outPorts =
  cFunDef "void" "outputAssigns"
          [("const " ++ topT ++ "*", "top"), ("uint8_t*", "raw")]
          [snd $ foldl assignOutPort (0, empty) outPorts]
  where
    assignOutPort (off, doc) (nm, w) =
      ( off + w
      , doc $+$ cFunCall (text "bitmemcpy")
                         [ cCast "void*" (text "raw +" <+> int byteOff)
                         , int bitOff
                         , cCast "void*" (text $ "&(top->" ++ nm ++ ")")
                         , int 0
                         , int w ] <> semi )
      where (byteOff, bitOff) = quotRem off 8

-- | Derive the code for the main function of the simulation toplevel
defMain :: CType -> [InPort] -> [OutPort] -> Doc
defMain topT inPorts outPorts =
  cFunDef "int" "main" [("int", "argc"), ("char**", "argv")]
          [ cFunCall (text "Verilated::commandArgs")
                     (text <$> ["argc", "argv"]) <> semi
          , cDef (topT ++ "*", "top") (Just . cNew $ topT)
          , cDef ("uint8_t*", "simReq")
                 (Just . cCast "uint8_t*" $ cMalloc (int inReqByteW))
          , cDef ("uint8_t*", "simRsp")
                 (Just . cCast "uint8_t*" $ cMalloc (int inReqByteW))
          , createInChannel
          , createOutChannel
          , cDef ("bool", "done") (Just $ text "false")
          , cWhile (cNot . cOrs $ [ cFunCall (text "Verilated::gotFinish") []
                                  , text "done" ])
                   [ text ("printf (\">>> waiting for cmd\\n\");")
                   , deqInChannel "simReq"
                   , cMemCpy (text "simRsp + 1")
                             (text "simReq + 1")
                             (int 8) <> semi
                   , text ("printf (\">>> prepared rsp\\n\");")
                   , cSwitch (cDeref $ cCast "uint8_t*" (text "simReq"))
                             [ (int $ fromIntegral Evaluate, handleEvalCmd)
                             , (int $ fromIntegral Finish, handleFinishCmd) ]
                             (Just handleUnknownCmd)
                   , text ("printf (\">>> sending back rsp\\n\");")
                   , enqOutChannel "simRsp" ]
          , text ("printf (\">>> done simulating\\n\");")
          , destroyInChannel
          , destroyOutChannel
          , cFree (text "simRsp") <> semi
          , cFree (text "simReq") <> semi
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
    --
    ptrBufAt nm 0 = text nm
    ptrBufAt nm n = text nm <+> char '+' <+> int n
    -- event handles
    handleEvalCmd = [
        text ("static int n = 0; printf (\">>> eval cmd %d\\n\", n);")
      , cFunCall (text "inputAssigns")
                 [ text "top", ptrBufAt "simReq" 9 ] <> semi
      , cFunCall (cIndirectAccess "top" "eval") [] <> semi
      , cAssign (cDeref $ ptrBufAt "simRsp" 0)
                (int $ fromIntegral Evaluated) <> semi
      , cFunCall (text "outputAssigns")
                 [ text "top", ptrBufAt "simRsp" 9 ] <> semi
      , text "break" <> semi
      ]
    handleFinishCmd = [
        text ("printf (\">>> finish cmd\\n\");")
      , cAssign (cDeref $ ptrBufAt "simRsp" 0)
                (int $ fromIntegral Finished) <> semi
      , cAssign (text "done") (text "true") <> semi
      , text "break" <> semi
      ]
    handleUnknownCmd = [
        text ("printf (\">>> unknown cmd\\n\");")
      , cAssign (cDeref $ ptrBufAt "simRsp" 0)
                (int $ fromIntegral Unknown) <> semi
      , cAssign (text "done") (text "true") <> semi
      , text "break" <> semi
      ]

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
                              (text <$> [nm, arg]) <> semi
    destroyChannel = cFunCall (chanSym "destroy") [text nm] <> semi

-- | Generate a C++ verilator simulation toplevel
genVerilatorTop :: Circuit -> String
genVerilatorTop c = renderStyle style $ vcat [ headersDoc
                                             , defTopInputAssigns topT inPorts
                                             , defTopOutputAssigns topT outPorts
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
