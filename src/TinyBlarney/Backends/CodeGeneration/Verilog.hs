{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |

Module      : TinyBlarney.Backends.CodeGeneration.Verilog
Description : TinyBlarney's verilog code generation backend
Stability   : experimental

This backend generates verilog for a given TinyBlarney 'Circuit'.

-}

module TinyBlarney.Backends.CodeGeneration.Verilog (
  generateVerilog
, generateTopVerilog
) where

import TinyBlarney.Core as BC
import TinyBlarney.Misc.PrettyHelpers.Verilog as PPV

import Prelude hiding ((<>))
import Data.List
import Data.Map qualified as M
import Data.Array
import Data.Maybe
import Data.Foldable
import Data.Sequence qualified as Seq
import Control.Monad.Reader
import Numeric (showHex)
import Text.PrettyPrint

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.CodeGeneration.Verilog: " ++ m

-- exported API
--------------------------------------------------------------------------------

-- | Generate all Verilog code for a 'Circuit'
generateVerilog :: Circuit -> M.Map String String
generateVerilog c = M.fromList [ (c'.name, generateTopVerilog c')
                               | c' <- getAllUniqueCircuits c ]

-- | Generate Verilog code for the top entity of a 'Circuit'
generateTopVerilog :: Circuit -> String
generateTopVerilog = renderStyle style . prettyVerilogModule
  where style = Style { mode = PageMode
                      , lineLength = 80
                      , ribbonsPerLine = 1.1 }

-- Internal helpers
--------------------------------------------------------------------------------

-- pretty helpers
commaSep :: [Doc] -> Doc
commaSep = sep . punctuate comma

-- | helper to for verilog pretty printing
toVPDir :: PortDir -> VPortDir
toVPDir BC.In = PPV.In
toVPDir BC.Out = PPV.Out

-- | code generation for a Verilog module
prettyVerilogModule :: Circuit -> Doc
prettyVerilogModule Circuit{ backingImplementation = Netlist netlist, .. } =
  vModDef name ifcPorts [declDoc, instDoc, alwsDoc]
  where
    -- Interface ports
    ifcPort ctxt@CircuitLeafCtxt{..} = case (mInstanceId, ifc) of
      (Just nId, Port pDir w) ->
        (toVPDir pDir, w, fromMaybe fail $ M.lookup (nId, path) netnames)
      (_, _) -> fail
      where
        fail = err $ "Could not identify interface port, ctxt: " ++ show ctxt
                     ++ " - netnames: " ++ show netnames
    ifcPorts =
      onCircuitInterfaceLeaves ifcPort interface
    -- extract net names from the netlist
    netnames = netlistNames netlist
    -- Verilog declarations, instances and always blocks ...
    -- generate the 'Doc's for the netlist
    netDocs = genAllNetDocs Env { netlist = netlist
                                , netnames = netnames }
    -- extract the verilog statements
    declDoc = sep $ (\x -> x.decl) <$> netDocs
    instDoc = sep $ (\x -> x.inst) <$> netDocs
    alwsDoc = sep $ (\x -> x.alws) <$> netDocs
    rstDoc = sep $ (\x -> x.rst) <$> netDocs -- TODO use reset blocks

prettyVerilogModule circuit =
  err $ "cannot generate verilog for circuit without netlist - " ++ show circuit

--------------------------------------------------------------------------------
-- NetDocs helper type
data NetDocs = NetDocs { decl :: Doc
                       , inst :: Doc
                       , alws :: Doc
                       , rst  :: Doc } deriving Show

-- | NetDocs generator monad
type GenNetDocs = Reader Env

-- | NetDocs generator reader monad environment
data Env = Env { netlist :: Netlist
               , netnames :: M.Map NetOutput String }

genAllNetDocs :: Env -> [NetDocs]
genAllNetDocs env = runReader (mapM genNetDocs $ elems env.netlist) env

genNetDocs :: Net -> GenNetDocs NetDocs
genNetDocs n = do newDecl <- genNetDeclDoc n
                  newInst <- genNetInstDoc n
                  newAlws <- genNetAlwsDoc n
                  newRst  <- genNetRstDoc n
                  return NetDocs { decl = newDecl
                                 , inst = newInst
                                 , alws = newAlws
                                 , rst  = newRst }

-- | Code generation for Verilog declarations
genNetDeclDoc :: Net -> GenNetDocs Doc
genNetDeclDoc n = case n.primitive of
  (Constant k w) -> genIdentDecl Wire (IntInitVal k) w nOut
  (DontCare w) -> genIdentDecl Wire DontCareInitVal w nOut
  (And w) -> genIdentDecl Wire NoInitVal w nOut
  (Or w) -> genIdentDecl Wire NoInitVal w nOut
  (Xor w) -> genIdentDecl Wire NoInitVal w nOut
  (Invert w) -> genIdentDecl Wire NoInitVal w nOut
  (Concatenate w0 w1) -> genIdentDecl Wire NoInitVal (w0 + w1) nOut
  (Slice (hi, lo) _) -> genIdentDecl Wire NoInitVal (hi-lo) nOut
  (Custom c) ->
    sep <$> mapM (\(p, w) -> genIdentDecl Wire NoInitVal w (nId, p)) nOutsInfo
  _ -> return empty
  where nId = n.instanceId
        nOut = netOutput n
        nOutsInfo = netOutputsInfo n


-- | Code generation for Verilog instantiations
genNetInstDoc :: Net -> GenNetDocs Doc
genNetInstDoc n = case n.primitive of
  (And _) -> instPrim
  (Or _) -> instPrim
  (Xor _) -> instPrim
  (Invert _) -> instPrim
  (Concatenate _ _) -> instPrim
  (Slice _ _) -> instPrim
  (Custom Circuit{..}) -> do
    ins <- mapM genNetConnectionRep nConns
    outs <- mapM askIdent nOuts
    return $ vModInst name (name ++ "_net" ++ show n.instanceId)
                           (ins ++ (text <$> outs))
  (Interface ifc) -> do
    let rets = sortOn snd $ netInputs n
    let args = sortOn fst $ n.inputConnections
    sep <$> (zipWithM instConn rets args)
  _ -> return empty
  where
    nConns = snd <$> n.inputConnections
    nOuts = netOutputs n
    instPrim = do identDoc <- text <$> askIdent (netOutput n)
                  primDoc <- genPrimRep n.primitive nConns
                  return $ vAssign identDoc primDoc
    instConn nPort@(_, p0) (p1, nConn) | p0 == p1 = do
      identDoc <- text <$> askIdent nPort
      valDoc <- genNetConnectionRep nConn
      return $ vAssign identDoc valDoc
                                      | otherwise =
      err $ "mismatched exported signal for " ++ show n

-- | Code generation for Verilog always block statements
genNetAlwsDoc :: Net -> GenNetDocs Doc
genNetAlwsDoc n = case n.primitive of
  _ -> return empty

-- | Code generation for Verilog reset statements
genNetRstDoc :: Net -> GenNetDocs Doc
genNetRstDoc n = case n.primitive of
  _ -> return empty

--------------------------------------------------------------------------------

-- Verilog identifier declaration
genIdentDecl :: VWireOrReg -> VInitVal -> BitWidth -> NetOutput
             -> GenNetDocs Doc
genIdentDecl wireOrReg initVal w nOut = do
  ident <- askIdent nOut
  return $ vIdentDef ident wireOrReg w initVal

-- | Get a verilog identifier out of the net name map
askIdent :: NetPort -> GenNetDocs String
askIdent nPort = do
  env <- ask
  case M.lookup nPort env.netnames of
    Just name -> return name
    _ -> err $ "name for " ++ show nPort ++ " not in\n" ++ show env.netnames

genNetConnectionRep :: NetConnection -> GenNetDocs Doc
genNetConnectionRep (NetConnection netOut) = text <$> askIdent netOut
genNetConnectionRep (NetConnectionInlined p ins) = parens <$> genPrimRep p ins

genPrimRep :: Primitive -> [NetConnection] -> GenNetDocs Doc
genPrimRep prim ins = case (prim, ins) of
  (Constant k w, []) -> return $ vIntLit k w
  (DontCare w, []) -> return $ vDontCare w
  (And _, [x, y]) -> binOp "&" x y
  (Or _, [x, y]) -> binOp "|" x y
  (Xor _, [x, y]) -> binOp "^" x y
  (Invert _, [x]) -> unOp "~" x
  (Concatenate _ _, [x, y]) -> do
    xDoc <- genNetConnectionRep x
    yDoc <- genNetConnectionRep y
    return $ braces $ commaSep [xDoc, yDoc]
  (Slice (hi, lo) _, [x]) -> do
    xDoc <- genNetConnectionRep x
    let sliceDoc = if hi == lo then int hi else int hi <> colon <> int lo
    return $ xDoc <> brackets sliceDoc
  (_, _) -> err $ "unsupported Prim '" ++ show prim ++ "' encountered"
  where binOp op x y = do xDoc <- genNetConnectionRep x
                          yDoc <- genNetConnectionRep y
                          return $ xDoc <+> text op <+> yDoc
        unOp op x = do xDoc <- genNetConnectionRep x
                       return $ text op <> parens xDoc
