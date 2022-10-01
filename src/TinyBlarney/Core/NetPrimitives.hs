{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TinyBlarney.Core.NetPrimitives (
  InstanceId
, NetOutput
, prettyNetOutput
, Net (..)
, prettyNet
, NetlistArray
, Netlist (..)
, prettyNetlist
, NetPort (..)
, prettyNetPort
, Primitive (..)
, prettyPrimitive
, primInterface
, primInputPaths
, primOutputPaths
, primPretty
, ifcUnaryOp
, ifcBinaryOp
) where

import Data.Array
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

import TinyBlarney.Core.CircuitInterface

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.NetPrimitives: " ++ m

--------------------------------------------------------------------------------

-- | A type to identify a 'Net' output. 'NetOutput' is a
--   '(InstanceId, CircuitInterfacePath)' pair.
type NetOutput = (InstanceId, CircuitInterfacePath)

-- | Pretty print a 'NetOutput'.
prettyNetOutput :: NetOutput -> Doc
prettyNetOutput (i, cPath) =
  text "net" PP.<> int i PP.<> prettyCircuitInterfacePath cPath

-- | A type to represent a 'Net' input.
data NetPort =
    -- | Constructor wrapping another 'Net''s output.
    NetPort NetOutput
    -- | Constructor inlining a 'Primitive' operation and a '[NetPort]' list of
    --   its inputs.
  | NetPortInlined Primitive [NetPort]

-- | Pretty print a 'NetPort'.
prettyNetPort :: NetPort -> Doc
prettyNetPort (NetPort nOut) = prettyNetOutput nOut
prettyNetPort (NetPortInlined p ins) =
  text "Op" PP.<> parens (primPretty p PP.<> sep (prettyNetPort <$> ins))

-- | Show instance for 'NetPort'.
instance Show NetPort where
  show = render . prettyNetPort

-- | A type to represent a netlist node.
data Net =
  MkNet { instanceId :: InstanceId -- ^ a unique instance identifier
        , primitive  :: Primitive  -- ^ a primitive operation
        , inputPorts :: [NetPort]  -- ^ a list of inputs
        }

-- | Pretty print a 'Net'.
prettyNet :: Net -> Doc
prettyNet MkNet{..} = text "net" PP.<> int instanceId <+> sep xs
  where xs = [ primPretty primitive
             , case inputPorts of
                 [] -> text "No Inputs"
                 ys -> text "Inputs"
                       <+> braces (nest 2 (sep (prettyNetPort <$> ys))) ]

-- | Show instance for 'Net'.
instance Show Net where
  show = render . prettyNet

-- | A 'NetlistArray' type synonym for 'Array InstanceId Net'.
type NetlistArray = Array InstanceId Net
-- | A 'Netlist', represented as an 'Array InstanceId Net'.
newtype Netlist = Netlist { netlistArray :: NetlistArray }

-- | Pretty print a 'Netlist'.
prettyNetlist :: Netlist -> Doc
prettyNetlist nl = vcat (prettyNet <$> elems nl.netlistArray)

-- | Show instance for 'Netlist'.
instance Show Netlist where
  show = render . prettyNetlist

-- | Available primitive operations.
data Primitive =

    -- | @Constant k w@: a @w@-sized constant value @k@
    --
    --   [__inputs__]  no input
    --   [__outputs__] the @w@-bit constant value @k@
    Constant Integer BitWidth

    -- | @DontCare w@: a @w@-sized don't care value
    --
    --   [__inputs__]  no input
    --   [__outputs__] the @w@-bit don't care value
  | DontCare BitWidth

    -- | @And w@: a @w@-sized bitwise "and" of 2 operands
    --
    --   [__inputs__]  @[x, y]@, 2 @w@-bit operands
    --   [__outputs__] the @w@-bit bitwise "and" of @x@ and @y@
  | And BitWidth

    -- | @Or w@: a @w@-sized bitwise "or" of 2 operands
    --
    --   [__inputs__]  @[x, y]@, 2 @w@-bit operands
    --   [__outputs__] the @w@-bit bitwise "or" of @x@ and @y@
  | Or BitWidth

    -- | @Xor w@: a @w@-sized bitwise "xor" of 2 operands
    --
    --   [__inputs__]  @[x, y]@, 2 @w@-bit operands
    --   [__outputs__] the @w@-bit bitwise "xor" of @x@ and @y@
  | Xor BitWidth

    -- | @Invert w@: a ones' complement of an operand
    --
    --   [__inputs__]  @x@, a @w@-bit operand
    --   [__outputs__] the one's complement of @x@
  | Invert BitWidth

    -- | @Concatenate wx wy@: a @wx+wy@-sized concatenation of 2 operands
    --
    --   [__inputs__]  @[x, y]@, a @wx@-bit and a @wy@-bit @x@ and @y@ operands
    --   [__outputs__] the @wx+wy@-bit concatenation of @x@ and @y@
  | Concatenate BitWidth BitWidth

    -- | @Slice (hi, lo) w@: a @(hi-lo+1)@-bit wide slice of a @w@-bit operand
    --
    --   [__inputs__]  @x@, a @w@-bit operand
    --   [__outputs__] the @(hi-lo+1)@-bit wide slice of @x@ between bit indices
    --                 @hi@ and @lo@ (both included)
  | Slice (Int, Int) BitWidth

    -- | A custom component
  | Custom { name :: String -- ^ component's name
           , interface :: CircuitInterface -- ^ component's interface
           , mNetlist :: Maybe Netlist -- ^ potential netlist for the component
           }

    -- | A circuit interface primitive
    --   BVs with this primitive are flatten roots or flatten leaves based on
    --   the polarity of the port described.
    --   The intended use is to have a single Interface primitive per circuit,
    --   and have a rich associated 'CircuitInterface'.
    --   The embedded 'CircuitInterface''s polarity can be flipped to obtain the
    --   circuit's interface as perceived from the outside of the circuit
    --   (example:
    --     From a circuit environment's perspective (the context instanciating
    --     a circuit), an Interface with a PortIn "A" and a PortOut "B"
    --     describes a circuit which will consume values from its environment
    --     through its "A" port, and produce values through its "B" port.
    --     From within that circuit, the Interface should be flipped, and
    --     present "A" as a PortOut which produces values for the rest of the
    --     netlist to consume, and "B" as a PortIn which consumes values
    --     produced by the rest of the nets in the netlist.
    --     A 'Primitive' is associated with nets of a netlist, and for this
    --     reason, it is expected that the 'CircuitInterface' parameter to this
    --     constructor be the one with the second described polarity.
    --   )
  | Interface CircuitInterface

-- | Pretty print a 'Primitive'.
prettyPrimitive :: Primitive -> Doc
prettyPrimitive = primPretty

-- | Show instance for 'Primitive'.
instance Show Primitive where
  show = render . primPretty

-- | General information on a primitive.
data PrimitiveInfo = MkPrimitiveInfo {
  -- | The circuit interface of the primitive (its inputs and outputs...).
  interface :: CircuitInterface
  -- | The pretty printing 'Doc' for the primitive.
, prettyDoc :: Doc
}

-- | Get the 'CircuitInterface' of a 'Primitive'.
primInterface :: Primitive -> CircuitInterface
primInterface prim = (primInfo prim).interface

-- | Get the 'CircuitInterfacePath's of a 'Primitive''s inputs.
primInputPaths :: Primitive -> [CircuitInterfacePath]
primInputPaths = getPortInPaths . primInterface

-- | Get the 'CircuitInterfacePath's of a 'Primitive''s outputs.
primOutputPaths :: Primitive -> [CircuitInterfacePath]
primOutputPaths = getPortOutPaths . primInterface

-- | Pretty print a 'Primitive'.
primPretty :: Primitive -> Doc
primPretty = prettyDoc . primInfo

-- | 'CircuitInterface' for 1-input 1-output circuits a.k.a. unary op.
ifcUnaryOp :: BitWidth -> BitWidth -> CircuitInterface
ifcUnaryOp wIn wOut =
  Product [ metaDocString "Unary operation input" . metaNameHint "in" $
              Port In wIn
          , metaDocString "Unary operation output" . metaNameHint "out" $
              Port Out wOut ]

-- | 'CircuitInterface' for 2-inputs 1-output circuits a.k.a. binary op.
ifcBinaryOp :: BitWidth -> BitWidth -> BitWidth -> CircuitInterface
ifcBinaryOp w0 w1 wOut =
  Product [ metaDocString "Binary operation inputs" $
              metaNameHint "in0" (Port In w0) <> metaNameHint "in1" (Port In w1)
          , metaDocString "Binary operation output" $
              metaNameHint "out" (Port Out wOut) ]

-- | primitive doc pretty printing helper
pDoc :: Doc -> CircuitInterface -> Doc
pDoc d ifc =
  text "Prim " <> braces (sep [d, parens $ prettyCircuitInterface ifc])

-- | document 'PrimitiveInfo' for any 'Primitive'
primInfo :: Primitive -> PrimitiveInfo
primInfo (Constant k w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Constant " <+> integer k) ifc
} where ifc = metaNameHint "out" $ Port Out w
primInfo (DontCare w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "DontCare") ifc
} where ifc = metaNameHint "out" $ Port Out w
primInfo (And w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "And") ifc
} where ifc = ifcBinaryOp w w w
primInfo (Or w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Or") ifc
} where ifc = ifcBinaryOp w w w
primInfo (Xor w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Xor") ifc
} where ifc = ifcBinaryOp w w w
primInfo (Invert w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Invert") ifc
} where ifc = ifcUnaryOp w w
primInfo (Concatenate w0 w1) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Concatenate") ifc
} where ifc = ifcBinaryOp w0 w1 (w0+w1)
primInfo (Slice (hi, lo) w) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc =
    pDoc (text "Slice" PP.<> parens (int hi PP.<> comma <+> int lo)) ifc
} where ifc = ifcUnaryOp w (hi-lo+1)
primInfo p@Custom{..} = MkPrimitiveInfo {
  interface = p.interface
, prettyDoc =
    let nl = case mNetlist of Nothing -> text "No Netlist"
                              Just _ -> text "Some Netlist"
    in pDoc (text "Custom: " <+> text name <+> char '-' <+> nl) p.interface
}
primInfo (Interface ifc) = MkPrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Interface") ifc
}
