{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import Prelude qualified as P
import Data.List
import Data.Map qualified as Map
import Data.Array
import Data.Maybe
import Data.Foldable
import Data.Sequence qualified as Seq
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Numeric (showHex)
import Text.PrettyPrint

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.CodeGeneration.Verilog: " ++ m

-- exported API
--------------------------------------------------------------------------------

-- | Generate all Verilog code for a 'Circuit'
generateVerilog :: Circuit -> Map.Map String String
generateVerilog c = Map.fromList [ (c'.name, generateTopVerilog c')
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
        (toVPDir pDir, w, fromMaybe fail $ Map.lookup (nId, path) netnames)
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
    netDocs = getNetDocs Env { netlist = netlist
                             , netnames = netnames }
    -- extract the verilog statements
    declDoc = sep $ netDocs.decls
    instDoc = sep $ netDocs.insts
    alwsDoc = vcat [ vAlwaysBlock ((text "posedge" <+>) <$> sensitivity) stmts
                   | (sensitivity, stmts) <- netDocs.alws ]
    -- TODO handle resets

prettyVerilogModule circuit =
  err $ "cannot generate verilog for circuit without netlist - " ++ show circuit

--------------------------------------------------------------------------------
-- NetDocs helper type
data NetDocs = NetDocs { decls :: [Doc]
                       , insts :: [Doc]
                       , alws  :: [([Doc],[Doc])] } deriving Show

-- | NetDocs generator reader monad environment
data Env = Env { netlist :: Netlist
               , netnames :: Map.Map NetOutput String }

-- | Gather the 'NetDocs' from the 'Env' using the generator monad
getNetDocs :: Env -> NetDocs
getNetDocs env = runIdentity $ runReaderT (evalStateT (runGen genAllNets) mempty) env

-- | NetDocs generator monad
type GenR = Env
data GenS = GenS {
    decls :: Seq.Seq Doc
  , insts :: Seq.Seq Doc
  , alws  :: Map.Map [NetPort] (Seq.Seq Doc)
  } deriving Show
instance Semigroup GenS where
  x <> y = GenS { decls = x.decls Seq.>< y.decls
                , insts = x.insts Seq.>< y.insts
                , alws = Map.unionWith (Seq.><) x.alws y.alws }
instance Monoid GenS where
  mempty = GenS { decls = mempty
                , insts = mempty
                , alws  = mempty }
newtype Gen a = Gen {
  runGen :: StateT GenS (ReaderT GenR Identity) a
} deriving (Functor, Applicative, Monad, MonadReader GenR, MonadState GenS)

genAllNets :: Gen NetDocs
genAllNets = do
  env <- ask
  mapM_ addNet $ elems env.netlist
  s <- get
  alwsBlocks <- sequence [ do xs' <- mapM askIdent xs
                              return (text <$> xs', toList ys)
                         | (xs, ys) <- Map.toList s.alws ]
  return NetDocs { decls = toList s.decls
                 , insts = toList s.insts
                 , alws  = alwsBlocks }

addNet :: Net -> Gen ()
addNet n = do
  mNetDecl <- genNetDecl n
  mNetInst <- genNetInst n
  mNetAlws <- genNetAlws n
  modify' \s ->
    GenS { decls = maybe s.decls (Seq.<| s.decls) mNetDecl
         , insts = maybe s.insts (Seq.<| s.insts) mNetInst
         , alws  = maybe s.alws
                         (\(k, v) -> Map.insertWith (Seq.><) k v s.alws)
                         mNetAlws }

-- | Code generation for Verilog declarations
genNetDecl :: Net -> Gen (Maybe Doc)
genNetDecl n = case n.primitive of
  (Constant k w) -> Just <$> genIdentDecl Wire (IntInitVal k) w nOut
  (DontCare w) -> Just <$> genIdentDecl Wire DontCareInitVal w nOut
  (And w) -> Just <$> genIdentDecl Wire NoInitVal w nOut
  (Or w) -> Just <$> genIdentDecl Wire NoInitVal w nOut
  (Xor w) -> Just <$> genIdentDecl Wire NoInitVal w nOut
  (Invert w) -> Just <$> genIdentDecl Wire NoInitVal w nOut
  (Concatenate w0 w1) -> Just <$> genIdentDecl Wire NoInitVal (w0 + w1) nOut
  (Slice (hi, lo) _) -> Just <$> genIdentDecl Wire NoInitVal (hi-lo) nOut
  (Merge MStratOr _ w) -> Just <$> genIdentDecl Wire NoInitVal w nOut
  (Register _ w) -> Just <$> genIdentDecl Reg NoInitVal w nOut
  (Custom c) ->
    Just <$> sep <$> mapM (\(p, w) -> genIdentDecl Wire NoInitVal w (nId, p))
                          nOutsInfo
  _ -> return Nothing
  where nId = n.instanceId
        nOut = netOutput n
        nOutsInfo = netOutputsInfo n


-- | Code generation for Verilog instantiations
genNetInst :: Net -> Gen (Maybe Doc)
genNetInst n = case n.primitive of
  (And _) -> instPrim
  (Or _) -> instPrim
  (Xor _) -> instPrim
  (Invert _) -> instPrim
  (Concatenate _ _) -> instPrim
  (Slice _ _) -> instPrim
  (Merge MStratOr _ _) -> instPrim
  (Custom Circuit{..}) -> do
    ins <- mapM genNetConnectionRep nConns
    outs <- mapM askIdent nOuts
    return . Just $ vModInst name (name ++ "_net" ++ show n.instanceId)
                                  (ins ++ (text <$> outs))
  (Interface ifc) -> do
    let rets = sortOn snd $ netInputs n
    let args = sortOn fst $ n.inputConnections
    instConns <- sep <$> zipWithM instConn rets args
    return $ Just instConns
  _ -> return Nothing
  where
    nConns = snd <$> n.inputConnections
    nOuts = netOutputs n
    instPrim = do identDoc <- text <$> askIdent (netOutput n)
                  primDoc <- genPrimRep n.primitive nConns
                  return . Just $ vAssign identDoc primDoc
    instConn nPort@(_, p0) (p1, nConn) | p0 == p1 = do
      identDoc <- text <$> askIdent nPort
      valDoc <- genNetConnectionRep nConn
      return $ vAssign identDoc valDoc
                                      | otherwise =
      err $ "mismatched exported signal for " ++ show n

-- | Code generation for Verilog always block statements
genNetAlws :: Net -> Gen (Maybe ([NetPort], Seq.Seq Doc))
genNetAlws n = case n.primitive of
  (Register _ w) -> do
    valOut <- askIdent (netOutput n)
    let nConns = snd <$> n.inputConnections
    valIns <- mapM genNetConnectionRep nConns
    let stmt = text valOut <+> text "<=" <+> valIns !! 1 <> semi
    return $ Just (netConnectionPorts (nConns !! 0), Seq.singleton stmt)
  _ -> return Nothing

-- | Code generation for Verilog reset statements
genNetRst :: Net -> Gen (Maybe Doc)
genNetRst n = case n.primitive of
  (Register (Just _) w) -> err "TODO: Register with reset value"
  _ -> return Nothing

--------------------------------------------------------------------------------

-- Verilog identifier declaration
genIdentDecl :: VWireOrReg -> VInitVal -> BitWidth -> NetOutput
             -> Gen Doc
genIdentDecl wireOrReg initVal w nOut = do
  ident <- askIdent nOut
  return $ vIdentDef ident wireOrReg w initVal

-- | Get a verilog identifier out of the net name map
askIdent :: NetPort -> Gen String
askIdent nPort = do
  env <- ask
  case Map.lookup nPort env.netnames of
    Just name -> return name
    _ -> err $ "name for " ++ show nPort ++ " not in\n" ++ show env.netnames

genNetConnectionRep :: NetConnection -> Gen Doc
genNetConnectionRep (NetConnection netOut) = text <$> askIdent netOut
genNetConnectionRep (NetConnectionInlined p ins) = parens <$> genPrimRep p ins

genPrimRep :: Primitive -> [NetConnection] -> Gen Doc
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
    return $ vConcat [xDoc, yDoc]
  (Slice (hi, lo) _, [x]) -> do
    xDoc <- genNetConnectionRep x
    let sliceDoc = if hi == lo then int hi else int hi <> colon <> int lo
    return $ xDoc <> brackets sliceDoc
  (Merge MStratOr n w, ins) -> do
    let v_en en v = parens (vReplicate (int w) [en] <+> text "&" <+> v)
    let peel [] = []
        peel (en:v:rest) = v_en en v : peel rest
    ins' <- mapM genNetConnectionRep ins
    return $ (sep . punctuate (char '|')) (peel ins')
    --return if n > 0 then foldl1 (\x y -> x <+> "|" <+> y) (peel ins')
    --                else empty
  (_, _) -> err $ "unsupported Prim '" ++ show prim ++ "' encountered"
  where binOp op x y = do xDoc <- genNetConnectionRep x
                          yDoc <- genNetConnectionRep y
                          return $ xDoc <+> text op <+> yDoc
        unOp op x = do xDoc <- genNetConnectionRep x
                       return $ text op <> parens xDoc
