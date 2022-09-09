{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TinyBlarney.Core.NetPrimitives (
  InstanceId
, Net (..)
, prettyNet
, Netlist (..)
, prettyNetlist
, NetPort (..)
, prettyNetPort
, Primitive (..)
, prettyPrimitive
, PrimName
, PrimitiveInfo (..)
, ifcUnaryOp
, ifcBinaryOp
, primitiveInfo
) where

import Data.Array
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

import TinyBlarney.Core.CircuitInterface

type InstanceId = Int

data NetPort = NetPort InstanceId CircuitInterfacePath
             | NetPortInlined Primitive [NetPort]

prettyNetPort :: NetPort -> Doc
prettyNetPort (NetPort i cPath) =
  text "Net#" PP.<> int i PP.<> prettyCircuitInterfacePath cPath
prettyNetPort (NetPortInlined p ins) =
  text "Op>" PP.<> prettyPrimitive p PP.<> sep (prettyNetPort <$> ins)

instance Show NetPort where
  show = render . prettyNetPort

data Net = MkNet { instanceId :: InstanceId
                 , primitive  :: Primitive
                 , inputPorts :: [NetPort] }

prettyNet :: Net -> Doc
prettyNet MkNet{..} = text "Net#" PP.<> int instanceId <+> sep xs
  where xs = [ prettyPrimitive primitive
             , case inputPorts of
                 [] -> text "No Inputs"
                 ys -> text "Inputs"
                       <+> braces (nest 2 (sep (prettyNetPort <$> ys)))
             ]

instance Show Net where
  show = render . prettyNet

-- | A 'Netlist', represented as an 'Array InstanceId Net'
newtype Netlist = Netlist (Array InstanceId Net)

prettyNetlist :: Netlist -> Doc
prettyNetlist (Netlist nl) = vcat (prettyNet <$> elems nl)

instance Show Netlist where
  show = render . prettyNetlist

type PrimName = String

-- | tiny-blarney's language primitives
data Primitive =
    -- | sized constant value
    Constant Integer BitWidth
    -- | logical and of 2 inputs
  | And BitWidth
    -- | logical or of 2 inputs
  | Or BitWidth
    -- | custom primitive with possible custom netlist
  | Custom { name :: PrimName
           , interface :: CircuitInterface
           , mNetlist :: Maybe Netlist }
    -- | a circuit interface primitive
    --   BVs with this primitive are flatten roots or flatten leaves based on
    --   the polarity of the port described
    --   The embedded 'CircuitInterface''s polarity can be flipped to obtain the
    --   circuit's interface as perceived from the outside of the circuit
    --   (example:
    --     A ciruit with an Interface primitive with a PortIn "A" and a PortOut
    --     "B" produces values at A and consumes values at B.
    --     The environment using this circuit will produce into it through B and
    --     consume out of it from A.
    --   )
  | Interface CircuitInterface

prettyPrimitive :: Primitive -> Doc
prettyPrimitive = prettyDoc . primitiveInfo

instance Show Primitive where
  show = render . prettyPrimitive

-- | general information on a primitive
data PrimitiveInfo = MkPrimitiveInfo {
  -- | the circuit interface of the primitive (its inputs and outputs...)
  interface :: CircuitInterface
  -- | the pretty printing 'Doc' for the primitive
, prettyDoc :: Doc
}

-- | 'CircuitInterface' for 1-input 1-output circuits a.k.a. unary op.
ifcUnaryOp :: BitWidth -> CircuitInterface
ifcUnaryOp w = PortIn "in" w <> PortOut "out" w
--ifcUnaryOp w = Product [ metaDocString "Unary operation input" $
--                           PortIn "in" w
--                       , metaDocString "Unary operation output" $
--                           PortOut "out" w ]

-- | 'CircuitInterface' for 2-inputs 1-output circuits a.k.a. binary op.
ifcBinaryOp :: BitWidth -> CircuitInterface
ifcBinaryOp w = PortIn "in0" w <> PortIn "in1" w <> PortOut "out" w
--ifcBinaryOp w = Product [ metaDocString "Binary operation inputs" $
--                            PortIn "in0" w <> PortIn "in1" w
--                        , metaDocString "Binary operation output" $
--                            PortOut "out" w ]

-- | primitive doc pretty printing helper
pDoc :: Doc -> CircuitInterface -> Doc
pDoc d ifc =
  text "Prim " <> braces (sep [d, parens $ prettyCircuitInterface ifc])

-- | document 'PrimitiveInfo' for any 'Primitive'
primitiveInfo :: Primitive -> PrimitiveInfo
primitiveInfo (Constant k w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Constant " <+> integer k) ifc
} where ifc = PortOut "out" w
primitiveInfo (And w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "And") ifc
} where ifc = ifcBinaryOp w
primitiveInfo (Or w) = MkPrimitiveInfo {
  interface = ifcBinaryOp w
, prettyDoc = pDoc (text "Or") ifc
} where ifc = ifcBinaryOp w
primitiveInfo p@Custom{..} = MkPrimitiveInfo {
  interface = p.interface
, prettyDoc =
    let nl = case mNetlist of Nothing -> text "No Netlist"
                              Just _ -> text "Some Netlist"
    in pDoc (text "Custom: " <+> text name <+> char '-' <+> nl) p.interface
}
primitiveInfo (Interface ifc) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Interface") ifc
}
