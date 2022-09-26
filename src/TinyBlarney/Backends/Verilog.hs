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

import Data.List
import Data.Array
import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as Seq
import Control.Monad.Identity
import Control.Monad.Writer hiding (Product)
import Control.Monad.Reader
import Numeric (showHex)
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

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
dot = char '.'
spaces n = hcat $ replicate n space
hexInt n = text (showHex n "")
argStyle = sep . punctuate comma

-- derive name for interface port
ifcName :: String -> [String] -> String
ifcName dflt [] = dflt
ifcName _ xs = intercalate "_" (reverse xs)

ifcPort :: CircuitLeafCtxt -> (String, PortDir, BitWidth)
ifcPort MkCircuitLeafCtxt{..} = case ifc of
  Port In w -> (ifcName ("arg" ++ show idx) nameHints, In, w)
  Port Out w -> (ifcName ("retVal" ++ show idx) nameHints, Out, w)
  where idx = case path of _ :|> n -> show n
                           _ -> ""

ifcPorts :: CircuitInterface -> [(String, PortDir, BitWidth)]
ifcPorts = onCircuitInterfaceLeaves ifcPort

-- | code generation for a Verilog module
prettyVerilogModule :: Circuit -> Doc
prettyVerilogModule circuit =
  header $+$ nest 2 (vcat [declBlk, instBlk, alwsBlk]) $+$ footer
  where
  header = text "module" <+> text circuit.name
                         <+> parens (argStyle (modPort <$> ports))
                         PP.<> semi
  declBlk = declDocs
  instBlk = instDocs
  alwsBlk = if isEmpty alwsDocs then empty else
        text "always" <+> char '@' PP.<> parens (text "posedge clock")
    <+> text "begin" $+$ nest 2 (wrapRstBlk alwsDocs) $+$ text "end"
  wrapRstBlk blk = if isEmpty rstDocs then blk else
        text "if (reset) begin" $+$ nest 2 rstDocs
    $+$ text "end else begin" $+$ nest 2 blk $+$ text "end"
  footer = text "endmodule"
  -- CircuitInterface as a module argument list
  ports = ifcPorts . externalCircuitInterface $ circuit
  modPort (nm, pDir, w) = let tDir = case pDir of In -> "input"
                                                  Out -> "output"
    in text tDir <+> text "wire" <+> brackets (int (w-1) PP.<> text ":0")
                 <+> text nm
  -- generate the appropriate 'Doc's for the netlist
  netDocs = genAllNetDocs circuit.netlist
  declDocs = sep $ toList netDocs.decl
  instDocs = sep $ toList netDocs.inst
  alwsDocs = sep $ toList netDocs.alws
  rstDocs  = sep $ toList netDocs.rst

-- NetDocs helper type
data NetDocs = NetDocs { decl :: Seq.Seq Doc
                       , inst :: Seq.Seq Doc
                       , alws :: Seq.Seq Doc
                       , rst  :: Seq.Seq Doc } deriving Show

-- | 'NetDocs' is a 'Semigroup'
instance Semigroup NetDocs where
  nd0 <> nd1 = nd0 { decl = nd0.decl <> nd1.decl
                   , inst = nd0.inst <> nd1.inst
                   , alws = nd0.alws <> nd1.alws
                   , rst  = nd0.rst  <> nd1.rst }

-- | 'NetDocs' is a 'Monoid'
instance Monoid NetDocs where
  mempty = NetDocs { decl = mempty
                   , inst = mempty
                   , alws = mempty
                   , rst  = mempty }

newtype GenNetDocs a = GenNetDocs {
  unGenNetDocs :: ReaderT NetlistArray (WriterT NetDocs Identity) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader NetlistArray
           , MonadWriter NetDocs )

genAllNetDocs :: Netlist -> NetDocs
genAllNetDocs (Netlist nl) =
  runIdentity $ execWriterT (runReaderT (unGenNetDocs gen) nl)
  where gen = mapM genNetDocs (elems nl)

genNetDocs :: Net -> GenNetDocs ()
genNetDocs n = do newDecl <- genNetDeclDoc n
                  newInst <- genNetInstDoc n
                  newAlws <- genNetAlwsDoc n
                  newRst  <- genNetRstDoc n
                  tell NetDocs { decl = Seq.singleton newDecl
                               , inst = Seq.singleton newInst
                               , alws = Seq.singleton newAlws
                               , rst  = Seq.singleton newRst }

--------------------------------------------------------------------------------
-- | Code generation for Verilog declarations
genNetDeclDoc :: Net -> GenNetDocs Doc
genNetDeclDoc n = return case n.primitive of
  (Constant k w) -> declareIdent nOut Wire (IntInitVal k) w
  (DontCare w) -> declareIdent nOut Wire DontCareInitVal w
  (And w) -> declareIdent nOut Wire NoInitVal w
  (Or w) -> declareIdent nOut Wire NoInitVal w
  (Xor w) -> declareIdent nOut Wire NoInitVal w
  (Invert w) -> declareIdent nOut Wire NoInitVal w
  (Concatenate w0 w1) -> declareIdent nOut Wire NoInitVal (w0 + w1)
  (Slice _ w) -> declareIdent nOut Wire NoInitVal w
  (Interface ifc) -> sep (declIfcPort <$> getPortOuts ifc)
  _ -> mempty
  where nId = n.instanceId
        nOut = netOutput n
        declIfcPort (p, Port Out w) = declareIdent (nId, p) Wire NoInitVal w
        declIfcPort _ = err $ "unsupported interface net: " ++ show n

-- | Code generation for Verilog instantiations
genNetInstDoc :: Net -> GenNetDocs Doc
genNetInstDoc n = return case n.primitive of
  (And _) -> instPrim
  (Or _) -> instPrim
  (Xor _) -> instPrim
  (Invert _) -> instPrim
  (Concatenate _ _) -> instPrim
  (Slice _ _) -> instPrim
  (Interface ifc) -> sep (instIfcPort <$> getPorts ifc)
  _ -> mempty
  where nId = n.instanceId
        nOut = netOutput n
        instPrim = pAssign (pIdent nOut) (pPrim n.primitive n.inputPorts)
        instIfcPort (p, Port In w) =
          pAssign (text "TODO") (pNetPort $ netInput n)
        instIfcPort (p, Port Out w) = pAssign (pIdent (nId, p)) (text "TODO")
        instIfcPort _ = err $ "unsupported interface net: " ++ show n

-- | Code generation for Verilog always block statements
genNetAlwsDoc :: Net -> GenNetDocs Doc
genNetAlwsDoc n = return case n.primitive of
  _ -> mempty

-- | Code generation for Verilog reset statements
genNetRstDoc :: Net -> GenNetDocs Doc
genNetRstDoc n = return case n.primitive of
  _ -> mempty

--------------------------------------------------------------------------------
pIntLit :: Integer -> BitWidth -> Doc
pIntLit v w = int w <> text "'h" <> hexInt v
pDontCare :: BitWidth -> Doc
pDontCare w = int w <> text "'b" <> text (replicate w 'x')
pIdent :: NetOutput -> Doc
pIdent (instId, path) =
  text "net" <> int instId <> prettyCircuitInterfacePath path
pAssign :: Doc -> Doc -> Doc
pAssign lhs rhs = (text "assign" <+> lhs <+> equals <+> rhs) <> semi

pPrim :: Primitive -> [NetPort] -> Doc
pPrim (Constant k w) [] = pIntLit k w
pPrim (DontCare w) [] = pDontCare w
pPrim (And _) [x, y] = pNetPort x <+> char '&' <+> pNetPort y
pPrim (Or _) [x, y] = pNetPort x <+> char '|' <+> pNetPort y
pPrim (Xor _) [x, y] = pNetPort x <+> char '^' <+> pNetPort y
pPrim (Invert _) [x] = char '~' <> parens (pNetPort x)
pPrim (Concatenate _ _) [x, y] = braces $ (pNetPort x <> comma) <+> pNetPort y
pPrim (Slice (hi, lo) _) [x] =
  parens (pNetPort x) <> brackets (int hi <> colon <> int lo)
pPrim p _ = err $ "unsupported Prim '" ++ show p ++ "' encountered"

pNetPort :: NetPort -> Doc
pNetPort (NetPort netOut) = pIdent netOut
pNetPort (NetPortInlined p ins) = parens $ pPrim p ins

data WireOrReg = Wire | Reg
data InitVal = NoInitVal | IntInitVal Integer | DontCareInitVal
declareIdent :: NetOutput -> WireOrReg -> InitVal -> BitWidth -> Doc
declareIdent netOut wireOrReg initVal w =
  (wireOrRegDoc <+> widthDoc <+> pIdent netOut <+> initDoc) <> semi
  where
  wireOrRegDoc = case wireOrReg of Wire -> text "wire"
                                   Reg  -> text "reg"
  widthDoc = if w > 1 then brackets (int (w-1) <> text ":0") else empty
  initDoc = case initVal of
    NoInitVal -> empty
    IntInitVal x -> equals <+> pIntLit x w
    DontCareInitVal -> equals <+> pDontCare w
