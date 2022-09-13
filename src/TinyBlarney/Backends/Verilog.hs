{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Backends.Verilog (
  writeVerilogModule
) where

import TinyBlarney.Core

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
writeVerilogModule :: String -> Circuit -> String
writeVerilogModule nm c = render $ prettyVerilogModule nm c

-- Internal helpers
--------------------------------------------------------------------------------

-- pretty helpers
dot = char '.'
spaces n = hcat $ replicate n space
hexInt n = text (showHex n "")
argStyle = sep . punctuate comma

-- | code generation for a Verilog module
prettyVerilogModule :: String -> Circuit -> Doc
prettyVerilogModule nm Circuit{..} =
  header $+$ nest 2 (vcat [declBlk, instBlk, alwsBlk]) $+$ footer
  where
  header = text "module" <+> text nm <+> parens (modArgs interface) PP.<> semi
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
  modArgs (PortIn nm w) =
    text "input wire" <+> brackets (int (w-1) PP.<> text ":0") <+> text nm
  modArgs (PortOut nm w) =
    text "output wire" <+> brackets (int (w-1) PP.<> text ":0") <+> text nm
  modArgs (Product xs) = argStyle (modArgs <$> xs)
  modArgs _ = empty
  -- generate the appropriate 'Doc's for the netlist
  netDocs = genAllNetDocs netlist
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
  (Constant k w) -> declareIdent nOut Wire (IntVal k) w
  (And w) -> declareIdent nOut Wire NoVal w
  (Or w) -> declareIdent nOut Wire NoVal w
  (Interface ifc) -> sep (declIfcPort <$> getPortOuts ifc)
  _ -> mempty
  where nId = n.instanceId
        nOut = netOutput n
        declIfcPort (p, PortOut _ w) = declareIdent (nId, p) Wire NoVal w
        declIfcPort _ = err $ "unsupported interface net: " ++ show n

-- | Code generation for Verilog instantiations
genNetInstDoc :: Net -> GenNetDocs Doc
genNetInstDoc n = return case n.primitive of
  (And _) -> instPrim
  (Or _) -> instPrim
  (Interface ifc) -> sep (instIfcPort <$> getPorts ifc)
  _ -> mempty
  where nId = n.instanceId
        nOut = netOutput n
        instPrim = pAssign (pIdent nOut) (pPrim n.primitive n.inputPorts)
        instIfcPort (p, PortIn nm w) =
          pAssign (text nm) (pNetPort $ netInput n)
        instIfcPort (p, PortOut nm w) = pAssign (pIdent (nId, p)) (text nm)
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
pPrim (And _) [x, y] = pNetPort x <+> char '&' <+> pNetPort y
pPrim (Or _) [x, y] = pNetPort x <+> char '|' <+> pNetPort y
pPrim p _ = err $ "unsupported Prim '" ++ show p ++ "' encountered"

pNetPort :: NetPort -> Doc
pNetPort (NetPort netOut) = pIdent netOut
pNetPort (NetPortInlined p ins) = parens $ pPrim p ins

data WireOrReg = Wire | Reg
data InitVal = NoVal | IntVal Integer | DontCareVal
declareIdent :: NetOutput -> WireOrReg -> InitVal -> BitWidth -> Doc
declareIdent netOut wireOrReg initVal w =
  (wireOrRegDoc <+> widthDoc <+> pIdent netOut <+> initDoc) <> semi
  where
  wireOrRegDoc = case wireOrReg of Wire -> text "wire"
                                   Reg  -> text "reg"
  widthDoc = if w > 1 then brackets (int (w-1) <> text ":0") else empty
  initDoc = case initVal of
    NoVal -> empty
    IntVal x -> equals <+> pIntLit x w
    DontCareVal -> equals <+> pDontCare w
