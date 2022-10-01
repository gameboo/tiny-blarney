{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |

Module      : TinyBlarney.Backends.Verilog
Description : TinyBlarney's verilog backend
Stability   : experimental

This backend generates verilog for a given TinyBlarney 'Circuit'.

-}

module TinyBlarney.Backends.Verilog (
  writeVerilogModule
) where

import TinyBlarney.Core

import Prelude hiding ((<>))
import Data.List
import qualified Data.Map as M
import Data.Array
import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as Seq
import Control.Monad.Reader
import Numeric (showHex)
import Text.PrettyPrint

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Verilog: " ++ m

-- exported API
--------------------------------------------------------------------------------
-- | Generate Verilog code for a 'Circuit'
writeVerilogModule :: Circuit -> String
writeVerilogModule c = render $ prettyVerilogModule c

-- Internal helpers
--------------------------------------------------------------------------------

-- pretty helpers
spaces :: Int -> Doc
spaces n = hcat $ replicate n space
hexInt :: Integer -> Doc
hexInt n = text (showHex n "")
commaSep :: [Doc] -> Doc
commaSep = sep . punctuate comma

-- | code generation for a Verilog module
prettyVerilogModule :: Circuit -> Doc
prettyVerilogModule circuit@Circuit{..} =
  vcat [headerDoc, nest 2 $ vcat [declDoc, instDoc, alwsDoc], footerDoc]
  where
    -- Module header (module statement and interface ports)
    headerDoc = text "module" <+> text name <+> parens modArgs <> semi
    ifcPort ctxt@MkCircuitLeafCtxt{..} = case (mInstanceId, ifc) of
      (Just nId, Port pDir w) ->
        (fromMaybe (err errMsg) $ M.lookup (nId, path) netnames, pDir, w)
      (_, _) -> err errMsg
      where errMsg =
                 "Could not identify interface port, ctxt: " ++ show ctxt
              ++ " - netnames: " ++ show netnames
    ifcPorts =
      onCircuitInterfaceLeaves ifcPort $ externalCircuitInterface circuit
    ifcPortDoc (nm, pDir, w) =
      text vDir <+> text "wire" <+> brackets (int (w-1) <> text ":0")
                <+> text nm
      where vDir = case pDir of In -> "input"
                                Out -> "output"
    modArgs = commaSep (ifcPortDoc <$> ifcPorts)
    -- declarations of Nets
    declDoc = sep declDocs
    -- instanciation of Nets
    instDoc = sep instDocs
    -- triggered always_ff blocks
    alwsDoc = sep alwsDocs -- TODO reset block
    -- module footer
    footerDoc = text "emdmodule"
    -- generate names for the nets of the netlist
    netnames = netNames deriveNames netlist
    deriveNames dflt [] = dflt
    deriveNames    _ xs = intercalate "_" $ reverse xs
    -- generate the 'Doc's for the netlist
    netDocs = genAllNetDocs Env { netlist = (\(Netlist x) -> x) netlist
                                , netnames = netnames }
    declDocs = map (\NetDocs{..} -> decl) netDocs
    instDocs = map (\NetDocs{..} -> inst) netDocs
    alwsDocs = map (\NetDocs{..} -> alws) netDocs
    rstDocs = map (\NetDocs{..} -> rst) netDocs

--------------------------------------------------------------------------------
-- NetDocs helper type
data NetDocs = NetDocs { decl :: Doc
                       , inst :: Doc
                       , alws :: Doc
                       , rst  :: Doc } deriving Show

-- | NetDocs generator monad
type GenNetDocs = Reader Env

-- | NetDocs generator reader monad environment
data Env = Env { netlist :: NetlistArray
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
  (Constant k w) -> genIdentDecl nOut Wire (IntInitVal k) w
  (DontCare w) -> genIdentDecl nOut Wire DontCareInitVal w
  (And w) -> genIdentDecl nOut Wire NoInitVal w
  (Or w) -> genIdentDecl nOut Wire NoInitVal w
  (Xor w) -> genIdentDecl nOut Wire NoInitVal w
  (Invert w) -> genIdentDecl nOut Wire NoInitVal w
  (Concatenate w0 w1) -> genIdentDecl nOut Wire NoInitVal (w0 + w1)
  (Slice _ w) -> genIdentDecl nOut Wire NoInitVal w
  (Interface ifc) -> sep <$> mapM genIfcPortDecl (getPortOuts ifc)
  _ -> return empty
  where nId = n.instanceId
        nOut = netOutput n
        genIfcPortDecl (p, Port Out w) = genIdentDecl (nId, p) Wire NoInitVal w
        genIfcPortDecl _ = err $ "unsupported interface net: " ++ show n


-- | Code generation for Verilog instantiations
genNetInstDoc :: Net -> GenNetDocs Doc
genNetInstDoc n = case n.primitive of
  (And _) -> instPrim
  (Or _) -> instPrim
  (Xor _) -> instPrim
  (Invert _) -> instPrim
  (Concatenate _ _) -> instPrim
  (Slice _ _) -> instPrim
  (Interface ifc) -> do
    let rets = sortOn snd $ netInputsAsNetOutput n
    let vals = sortOn fst $ netInputs n
    sep <$> (zipWithM instPort rets vals)
  _ -> return empty
  where
    instPrim = do identDoc <- askIdent $ netOutput n
                  primDoc <- genPrimRep n.primitive (snd <$> n.inputPorts)
                  return $ vAssign identDoc primDoc
    instPort nOut@(_, p0) (p1, nPort) | p0 == p1 = do
      identDoc <- askIdent nOut
      valDoc <- genNetPortRep nPort
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

-- | Verilog Wire or Register
data WireOrReg = Wire | Reg
-- | Initialization value type
data InitVal = NoInitVal | IntInitVal Integer | DontCareInitVal

-- Verilog identifier declaration
genIdentDecl :: NetOutput -> WireOrReg -> InitVal -> BitWidth
             -> GenNetDocs Doc
genIdentDecl nOut wireOrReg initVal w = do
  identDoc <- askIdent nOut
  return $ wireOrRegDoc <+> widthDoc <+> identDoc <+> initDoc <> semi
  where
  wireOrRegDoc = case wireOrReg of Wire -> text "wire"
                                   Reg  -> text "reg"
  widthDoc = if w > 1 then brackets (int (w-1) <> text ":0") else mempty
  initDoc = case initVal of
    NoInitVal -> mempty
    IntInitVal x -> equals <+> vIntLit x w
    DontCareInitVal -> equals <+> vDontCare w

-- | Get a verilog identifier out of the net name map
askIdent :: NetOutput -> GenNetDocs Doc
askIdent nOut = do
  env <- ask
  let Just name = M.lookup nOut env.netnames
  return $ text name

genNetPortRep :: NetPort -> GenNetDocs Doc
genNetPortRep (NetPort netOut) = askIdent netOut
genNetPortRep (NetPortInlined p ins) = parens <$> genPrimRep p ins

genPrimRep :: Primitive -> [NetPort] -> GenNetDocs Doc
genPrimRep prim ins = case (prim, ins) of
  (Constant k w, []) -> return $ vIntLit k w
  (DontCare w, []) -> return $ vDontCare w
  (And _, [x, y]) -> binOp "&" x y
  (Or _, [x, y]) -> binOp "|" x y
  (Xor _, [x, y]) -> binOp "^" x y
  (Invert _, [x]) -> unOp "~" x
  (Concatenate _ _, [x, y]) -> do
    xDoc <- genNetPortRep x
    yDoc <- genNetPortRep y
    return $ braces $ commaSep [xDoc, yDoc]
  (Slice (hi, lo) _, [x]) -> do
    xDoc <- genNetPortRep x
    return $ parens xDoc <> brackets (int hi <> colon <> int lo)
  (_, _) -> err $ "unsupported Prim '" ++ show prim ++ "' encountered"
  where binOp op x y = do xDoc <- genNetPortRep x
                          yDoc <- genNetPortRep y
                          return $ xDoc <+> text op <+> yDoc
        unOp op x = do xDoc <- genNetPortRep x
                       return $ text op <> parens xDoc

--------------------------------------------------------------------------------
-- Verilog helpers
vIntLit :: Integer -> BitWidth -> Doc
vIntLit v w = int w <> text "'h" <> hexInt v
vDontCare :: BitWidth -> Doc
vDontCare w = int w <> text "'b" <> text (replicate w 'x')
vAssign :: Doc -> Doc -> Doc
vAssign lhs rhs = (text "assign" <+> lhs <+> equals <+> rhs) <> semi
