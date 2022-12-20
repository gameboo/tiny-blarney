{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- |

Module      : TinyBlarney.Core.BasicTypes
Description : TinyBlarney's basic types used in its "core" module
Stability   : experimental

-}

module TinyBlarney.Core.BasicTypes (
  module TinyBlarney.Core.CircuitInterface
  -- * types
, Primitive (..)
, MergeStrategy (..)
, Net (..)
, NetPort
, NetInput
, NetOutput
, NetConnection (..)
, NetInputConnection
, Netlist
, Circuit (..)
, CircuitImplementation (..)
, Backend (..)
  -- * pretty printers
, prettyPrimitive
, prettyNet
, prettyNetPort
, prettyNetInput
, prettyNetOutput
, prettyNetConnection
, prettyNetInputConnection
, prettyCircuit
, prettyCircuitImplementation
  -- * basic operations
, getChildrenCircuits
, getUniqueChildrenCircuits
, getAllCircuits
, getAllUniqueCircuits
, primInterface
, primInputsInfo
, primInputWidths
, primInputPaths
, primOutputsInfo
, primOutputWidths
, primOutputPaths
, primPretty
, primEval
, primEvalFirst
, ifcUnaryOp
, ifcBinaryOp
) where

import Data.List
import Data.Bits hiding (And, Xor)
import Data.Array
import qualified Data.Map as M
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

import TinyBlarney.Misc.Misc
import TinyBlarney.Core.CircuitInterface

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.BasicTypes: " ++ m

--------------------------------------------------------------------------------
-- Basic types

--------------------------------------------------------------------------------
-- | A type to identify a 'Net' port. 'NetPort' is a
--   '(InstanceId, CircuitInterfacePath)' pair.
type NetPort = (InstanceId, CircuitInterfacePath)
type NetInput = NetPort
type NetOutput = NetPort

-- | Pretty print a 'NetPort'.
prettyNetPort :: NetPort -> Doc
prettyNetPort (i, cPath) =
  text "net" PP.<> int i PP.<> prettyCircuitInterfacePath cPath

-- | Pretty print a 'NetInput'.
prettyNetInput :: NetInput -> Doc
prettyNetInput = prettyNetPort

-- | Pretty print a 'NetOutput'.
prettyNetOutput :: NetOutput -> Doc
prettyNetOutput = prettyNetPort

--------------------------------------------------------------------------------
-- | A type to represent the connection to a 'Net' input port. It holds
--   information about the 'NetOutput' on the other end of the connection if
--   any and on inlined operations if any.
data NetConnection =
    -- | Constructor wrapping another 'Net''s output.
    NetConnection NetOutput
    -- | Constructor inlining a 'Primitive' operation and a '[NetConnection]'
    --   list of its inputs.
  | NetConnectionInlined Primitive [NetConnection]

-- | Pretty print a 'NetConnection'.
prettyNetConnection :: NetConnection -> Doc
prettyNetConnection (NetConnection nOut) = prettyNetOutput nOut
prettyNetConnection (NetConnectionInlined p ncs) =
  text "Op" PP.<> parens (primPretty p PP.<> sep (prettyNetConnection <$> ncs))

-- | Show instance for 'NetConnection'.
instance Show NetConnection where
  show = render . prettyNetConnection

--------------------------------------------------------------------------------
-- | A type to refer to an input port's connection. 'NetInputConnection' is a
--   '(CircuitInterfacePath, NetConnection)' pair.
type NetInputConnection = (CircuitInterfacePath, NetConnection)

-- | Pretty print a 'NetInputConnection'.
prettyNetInputConnection :: NetInputConnection -> Doc
prettyNetInputConnection (cPath, nConn) =
  prettyCircuitInterfacePath cPath <+> text ":=" <+> prettyNetConnection nConn

--------------------------------------------------------------------------------
-- | A type to represent a netlist node.
data Net = Net { -- | a unique instance identifier
                 instanceId :: InstanceId
                 -- | a primitive operation
               , primitive :: Primitive
                 -- | a list of input connections
               , inputConnections :: [NetInputConnection]
               }

-- | Pretty print a 'Net'.
prettyNet :: Net -> Doc
prettyNet Net{..} = text "net" PP.<> int instanceId <+> sep xs
  where
    xs = [ primPretty primitive
         , case inputConnections of
             [] -> text "No Inputs"
             ys -> text "Inputs"
                   <+> braces (nest 2 $ ppConns ys) ]
    ppConns = sep . punctuate comma . fmap prettyNetInputConnection

-- | Show instance for 'Net'.
instance Show Net where
  show = render . prettyNet

--------------------------------------------------------------------------------
-- | A 'Netlist' type synonym for 'Array InstanceId Net'.
type Netlist = Array InstanceId Net

--------------------------------------------------------------------------------
-- A type to express the backing implementation of a TinyBlarney 'Circuit'
data CircuitImplementation =
    Netlist Netlist -- ^ a 'TinyBlarney 'Netlist'
  | BackendFiles [(Backend, FilePath)] -- ^ a list of files for given backends

-- | Pretty print a 'CircuitImplementation'.
prettyCircuitImplementation :: CircuitImplementation -> Doc
prettyCircuitImplementation (Netlist nl) = vcat (prettyNet <$> elems nl)
prettyCircuitImplementation (BackendFiles []) = text "No backing implementation"
prettyCircuitImplementation (BackendFiles xs) =
  vcat [text (show bckEnd) PP.<> colon <+> text fPath | (bckEnd, fPath) <- xs]

--------------------------------------------------------------------------------
-- | Show instance for 'CircuitImplementation'.
instance Show CircuitImplementation where
  show = render . prettyCircuitImplementation

--------------------------------------------------------------------------------
-- | A type to identify a TinyBlarney backend
data Backend = Verilog -- ^ TinyBlarney's verilog backend
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | A built 'Circuit' with a name and a 'CircuitInterface', and possibly a
--  'Netlist' or an implementation for a given backend
data Circuit = Circuit { -- | circuit's name
                         name :: String
                         -- | circuit's interface
                       , interface :: CircuitInterface
                         -- | backing implementation of a 'Circuit'
                       , backingImplementation :: CircuitImplementation
                       }

-- | Pretty-print a 'Circuit'
prettyCircuit :: Circuit -> Doc
prettyCircuit circuit =
  hang (text "Circuit -" <+> text circuit.name) 2 (pIfc $+$ pImpls)
  where
    pIfc =
      text "- interface:" <+> nest 2 (prettyCircuitInterface circuit.interface)
    pImpls = prettyCircuitImplementation circuit.backingImplementation

-- | 'Show' instance for 'Circuit'
instance Show Circuit where
  show = render . prettyCircuit

-- | Retreive children 'Circuit's
getChildrenCircuits :: Circuit -> [Circuit]
getChildrenCircuits c@Circuit{ backingImplementation = Netlist nl } =
  [ c' | Net {primitive = Custom c'} <- elems nl ]
getChildrenCircuits _ = []

-- | Retreive children 'Circuit's removing duplicates
getUniqueChildrenCircuits :: Circuit -> [Circuit]
getUniqueChildrenCircuits c = uniq cs
  where uniq = fmap head . groupBy sameName . sortOn name
        sameName c0 c1 = c0.name == c1.name
        cs = getChildrenCircuits c

-- | Retreive all available nested 'Circuit's in a 'Circuit'
getAllCircuits :: Circuit -> [Circuit]
getAllCircuits c@Circuit{ backingImplementation = Netlist nl } =
  c : concat [ getAllCircuits c' | Net {primitive = Custom c'} <- elems nl ]
getAllCircuits c = [c]

-- | Retreive all available nested 'Circuit's in a 'Circuit' removing duplicates
getAllUniqueCircuits :: Circuit -> [Circuit]
getAllUniqueCircuits c = uniq cs
  where uniq = fmap head . groupBy sameName . sortOn name
        sameName c0 c1 = c0.name == c1.name
        cs = getAllCircuits c

--------------------------------------------------------------------------------
-- | Merging Strategy
data MergeStrategy = MStratOr deriving (Eq, Show)

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

    -- | @Merge mStrat n w@ represents a merging primitive with the @mStrat@
    --   merging strategy, @n@ pairs of 1-bit enables and associated @w@-wide
    --   inputs, and one @w@-wide output
    --
    --   [__inputs__]  @[en0, in0, en1, in1, ...]@, @n@ pairs of @enN@ 1-bit
    --                 enables and @inN@ @w@-bit values
    --   [__outputs__] a single output, a @w@-bit value result of the merging of
    --                 the @n@ inputs according to @mStrat@
  | Merge MergeStrategy Int BitWidth

    -- | A custom component
  | Custom Circuit

    -- | A circuit interface primitive.
    --   'BV's with this primitive are flatten roots and/or flatten leaves
    --   (based on the direction of the port(s) described).
    --   The intended use for this primitive is to have exactly one instance of
    --   it per circuit, with a rich associated 'CircuitInterface'.
    --   The 'CircuitInterface' argument describes the primitive's interface as
    --   perceived by the rest of the circuit. The direction of the ports in
    --   this embedded 'CircuitInterface' can be flipped to obtain the circuit's
    --   interface as perceived from the outside of the circuit
    --   (example:
    --     From a circuit environment's perspective (the context instanciating
    --     a circuit), an Interface with an input port "A" and an output port
    --     "B" describes a circuit which will consume values from its
    --     environment through its "A" port, and produce values through its "B"
    --     port. From within that circuit, the Interface is flipped, and
    --     presents "A" as an output port which produces values for the rest of
    --     the netlist to consume, and "B" as an input port which consumes
    --     values produced by the rest of the nets in the netlist.
    --   )
  | Interface CircuitInterface

-- | Pretty print a 'Primitive'.
prettyPrimitive :: Primitive -> Doc
prettyPrimitive = primPretty

-- | Show instance for 'Primitive'.
instance Show Primitive where
  show = render . primPretty

-- | General information on a primitive.
data PrimitiveInfo = PrimitiveInfo {
  -- | The circuit interface of the primitive (its inputs and outputs...).
  interface :: CircuitInterface
  -- | The pretty printing 'Doc' for the primitive.
, prettyDoc :: Doc
  -- | Evaluate the primitive given integer input values, and return a list of
  --   evaluated outputs or an empty list if evaluation is not possible.
, evaluate :: [(CircuitInterfacePath, Integer)]
           -> [(CircuitInterfacePath, Integer)]
}

-- | Get the 'CircuitInterface' of a 'Primitive'.
primInterface :: Primitive -> CircuitInterface
primInterface prim = (primInfo prim).interface

-- | Get general info about a 'Primitive''s inputs.
primInputsInfo :: Primitive -> [(CircuitInterfacePath, BitWidth)]
primInputsInfo = getPortInsInfo . primInterface

-- | Get the 'BitWidth's of a 'Primitive''s inputs.
primInputWidths :: Primitive -> [BitWidth]
primInputWidths = map snd . primInputsInfo

-- | Get the 'CircuitInterfacePath's of a 'Primitive''s inputs.
primInputPaths :: Primitive -> [CircuitInterfacePath]
primInputPaths = map fst . primInputsInfo

-- | Get general info about a 'Primitive''s outputs.
primOutputsInfo :: Primitive -> [(CircuitInterfacePath, BitWidth)]
primOutputsInfo = getPortOutsInfo . primInterface

-- | Get the 'BitWidth's of a 'Primitive''s outputs.
primOutputWidths :: Primitive -> [BitWidth]
primOutputWidths = map snd . primOutputsInfo

-- | Get the 'CircuitInterfacePath's of a 'Primitive''s outputs.
primOutputPaths :: Primitive -> [CircuitInterfacePath]
primOutputPaths = map fst . primOutputsInfo

-- | Pretty print a 'Primitive'.
primPretty :: Primitive -> Doc
primPretty = prettyDoc . primInfo

-- | Evaluate a 'Primitive'.
primEval :: Primitive -> (    [(CircuitInterfacePath, Integer)]
                           -> [(CircuitInterfacePath, Integer)] )
primEval = evaluate . primInfo

-- | Evaluate the first output of a 'Primitive'.
primEvalFirst :: Primitive
              -> ([(CircuitInterfacePath, Integer)] -> Maybe Integer)
primEvalFirst p = \ins -> case primEval p ins of (_, x):xs -> Just x
                                                 _ -> Nothing

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

-- local helper to grab the first output path of an interface
outPath :: CircuitInterface -> CircuitInterfacePath
outPath = head . getPortOutPaths

-- | document 'PrimitiveInfo' for any 'Primitive'
primInfo :: Primitive -> PrimitiveInfo
primInfo (Constant k w) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Constant " <+> integer k) ifc
, evaluate = \_ -> [(outPath ifc, toInteger k)]
} where ifc = metaNameHint "out" $ Port Out w
primInfo (DontCare w) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "DontCare") ifc
, evaluate = \_ -> [(outPath ifc, 0)]
} where ifc = metaNameHint "out" $ Port Out w
primInfo (And w) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "And") ifc
, evaluate = \case
    [(_, x), (_, y)] -> [(outPath ifc, x .&. y)]
    _ -> []
} where ifc = ifcBinaryOp w w w
primInfo (Or w) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Or") ifc
, evaluate = \case
    [(_, x), (_, y)] -> [(outPath ifc, x .|. y)]
    _ -> []
} where ifc = ifcBinaryOp w w w
primInfo (Xor w) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Xor") ifc
, evaluate = \case
    [(_, x), (_, y)] -> [(outPath ifc, x `xor` y)]
    _ -> []
} where ifc = ifcBinaryOp w w w
primInfo (Invert w) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Invert") ifc
, evaluate = \case
    [(_, x)] -> [(outPath ifc, complement x)]
    _ -> []
} where ifc = ifcUnaryOp w w
primInfo (Concatenate w0 w1) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Concatenate") ifc
, evaluate = \case
    [(_, x), (_, y)] -> [(outPath ifc, x `shiftL` w1 .|. y)]
    _ -> []
} where ifc = ifcBinaryOp w0 w1 (w0+w1)
primInfo (Slice (hi, lo) w) = PrimitiveInfo {
  interface = ifc
, prettyDoc =
    pDoc (text "Slice" PP.<> parens (int hi PP.<> comma <+> int lo)) ifc
, evaluate = \case
    [(_, x)] -> [(outPath ifc, clamp (hi + 1) x `shiftR` lo)]
    _ -> []
} where ifc = ifcUnaryOp w (hi-lo+1)
primInfo (Merge mStrat n w) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text $ "Prim Merge." ++ show mStrat) ifc
, evaluate = \_ -> []
} where ifc =    metaNameHint (show mStrat ++ "Input")
                              (Product [enIn i | i <- [0..(n-1)]])
              <> metaNameHint (show mStrat ++ "Output") (Port Out w)
        enIn i =    metaNameHint ("en" ++ show i) (Port In 1)
                 <> metaNameHint ("in" ++ show i) (Port In w)
primInfo (Custom circuit) = PrimitiveInfo {
  interface = circuit.interface
, prettyDoc = text "Prim Custom: " <+> prettyCircuit circuit
, evaluate = \_ -> []
}
primInfo (Interface ifc) = PrimitiveInfo {
  interface = ifc
, prettyDoc = pDoc (text "Interface") ifc
, evaluate = \_ -> []
}
