-- This file is an exercise in having a minimal blarney-like setup available
-- for exploration of possible new features / rewrites / etc... which would be
-- costly to explore in the main blarney setup
--
-- It limits itself to a reduced selections of primitives, but still tries to
-- include one from each "interesting" primitives...
--
-- Currently, it is used to explore the notion of a more advanced port
-- data structure.
--
-- tiny-blarney does not necessarily concern itself with efficient
-- implementation of its features (inefficient list representation for the
-- netlist, etc...)
--
-- tiny-blarney provides:
-- * data structures for port description
-- * untyped bit vectors in an infinite tree fashion (but with embedded unique
--   identifiers (using unsafePerformIO) for observable sharing to allow
--   flattening to a graph)
-- * nets in a graph fashion, flattened from the untyped bit vector tree
-- * a simple verilog backend XXX TODO
--
-- tiny-blarney does not provide:
-- * typed bit vectors
-- * a nice RTL monad
-- * a simulation backend
-- * an SMT backend
-- * nice utility libraries of components to compose with

{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE DuplicateRecordFields  #-}
-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module TinyBlarney where

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Trans
import qualified Data.IntSet as IntSet
import Data.Functor.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.Map as Map
import Text.PrettyPrint hiding ((<>))

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

type IfcName = String
type BitWidth = Int
type PrimName = String
type InstanceId = Int

data CircuitInterface =
  -- ** leaf 'CircuitInterface' constructors
    PortIn IfcName BitWidth
  | PortOut IfcName BitWidth
--  | StaticParamInt IfcName Integer
--  | StaticParamString IfcName String
--  | PortInOut IfcName BitWidth
--  | ClockIn IfcName
--  | ClockOut IfcName
--  | ResetIn IfcName
--  | ResetOut IfcName
  -- ** non-leaf constructors
  | Product [CircuitInterface]
  | Meta MetaInfo CircuitInterface
  deriving Show

isCircuitInterfaceLeaf :: CircuitInterface -> Bool
isCircuitInterfaceLeaf (Meta _ _) = False
isCircuitInterfaceLeaf (Product _) = False
isCircuitInterfaceLeaf _ = True

flipCircuitInterface :: CircuitInterface -> CircuitInterface
flipCircuitInterface (PortIn nm w) = PortOut nm w
flipCircuitInterface (PortOut nm w) = PortIn nm w
flipCircuitInterface (Meta m x) = Meta m $ flipCircuitInterface x
flipCircuitInterface (Product xs) = Product $ flipCircuitInterface <$> xs
--flipCircuitInterface x = x

data MetaInfo = NameHint String deriving Show

-- | 'CircuitInterface' is a 'Semigroup'
instance Semigroup CircuitInterface where
  Product xs <> Product ys = Product $ xs ++ ys
  Product xs <> y = Product $ xs ++ [y]
  x <> Product ys = Product $ x : ys
  x <> y = Product [x, y]

-- | 'CircuitInterface' is a 'Monoid'
instance Monoid CircuitInterface where
  mempty = Product []

type CircuitInterfacePath = [Int]
type Query a = CircuitInterface -> Maybe a
type Transformation = CircuitInterface -> Maybe CircuitInterface

getPortOutBitWidth :: Query BitWidth
getPortOutBitWidth (PortOut _ w) = Just w
getPortOutBitWidth _ = Nothing

queryCircuitInterfaceAt :: Query a -> CircuitInterface -> CircuitInterfacePath
                        -> Maybe a
queryCircuitInterfaceAt query x [] = query x
queryCircuitInterfaceAt query (Meta _ x) steps =
  queryCircuitInterfaceAt query x steps
queryCircuitInterfaceAt query (Product xs) (n : steps) | n < length xs =
  queryCircuitInterfaceAt query (xs !! n) steps
queryCircuitInterfaceAt _ _ _ = Nothing

queryCircuitInterfaceLeaves :: Query a -> CircuitInterface
                            -> [(CircuitInterfacePath, Maybe a)]
queryCircuitInterfaceLeaves query ifc = go [] query ifc
  where go stps query (Meta _ x) = go stps query x
        go stps query (Product xs) =
          concat [go (n : stps) query x | (n, x) <- zip [0..] xs]
        go stps query x | isCircuitInterfaceLeaf x = [(reverse stps, query x)]
        go _ _ _ = []

getPortIns :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPortIns ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortIn ifc]
  where exposePortIn p@(PortIn _ _) = Just p
        exposePortIn _ = Nothing

getPortOuts :: CircuitInterface -> [(CircuitInterfacePath, CircuitInterface)]
getPortOuts ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOut ifc ]
  where exposePortOut p@(PortOut _ _) = Just p
        exposePortOut _ = Nothing

getPortOutWidths :: CircuitInterface -> [(CircuitInterfacePath, BitWidth)]
getPortOutWidths ifc =
  [ (x, y) | (x, Just y) <- queryCircuitInterfaceLeaves exposePortOutW ifc ]
  where exposePortOutW (PortOut _ w) = Just w
        exposePortOutW _ = Nothing

--------------------------------------------------------------------------------
-- ** tiny-blarney language primitives

-- | strategy used by a 'PrimMergeWrites' primitive to merge written values
data MergeStrategy = MergeStratOr deriving Show

newtype CustomNetlist = MkCustomNetlist () -- TODO
instance Show CustomNetlist where show _ = "CustomNetlist"

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
           , mNetlist :: Maybe CustomNetlist }
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
  deriving Show

-- | general information on a primitive
data PrimitiveInfo = MkPrimitiveInfo {
  -- | the circuit description of the primitive (its inputs and outputs...)
  circuitInterface :: CircuitInterface
}

-- | 'CircuitInterface' for 0-input 1-output circuits
ifc0in1out :: BitWidth -> CircuitInterface
ifc0in1out w = PortOut "out" w

-- | 'CircuitInterface' for 1-input 1-output circuits a.k.a. unary op.
ifc1in1out :: BitWidth -> CircuitInterface
ifc1in1out w = PortIn "in" w <> PortOut "out" w
ifcUnaryOp = ifc1in1out

-- | 'CircuitInterface' for 2-inputs 1-output circuits a.k.a. binary op.
ifc2in1out :: BitWidth -> CircuitInterface
ifc2in1out w = PortIn "in0" w <> PortIn "in1" w <> PortOut "out" w
ifcBinaryOp = ifc2in1out

-- | 'CircuitInterface' for 1-input 0-output circuits
ifc1in0out :: BitWidth -> CircuitInterface
ifc1in0out w = PortIn "in" w

-- | document 'PrimitiveInfo' for any 'Primitive'
primitiveInfo :: Primitive -> PrimitiveInfo
primitiveInfo (Constant _ w) =
  MkPrimitiveInfo { circuitInterface = ifc0in1out w }
primitiveInfo (And w) = MkPrimitiveInfo { circuitInterface = ifcBinaryOp w }
primitiveInfo (Or w) = MkPrimitiveInfo { circuitInterface = ifcBinaryOp w }
primitiveInfo p@Custom{} =
  MkPrimitiveInfo { circuitInterface = p.interface }
primitiveInfo (Interface ifc) = MkPrimitiveInfo { circuitInterface = ifc }

--------------------------------------------------------------------------------
-- untyped bitvector
-- A BV refer to one output port of a circuit description of a primitive.
-- Primitives with multiple output ports in their circuit discription will
-- require multiple BVs. These BVs will share their instanceId
-- (observable sharing)
-- An output port should be uniquely identifiable with an instanceId and a
-- portName (the instanceId isolate a specific PrimitiveCircuitDescription
-- instance, and the PortName should identify a unique output port in this
-- PrimitiveCircuitDescription)

data BV = MkBV { instanceId :: InstanceId
               , primitive :: Primitive
               , receivedSignals :: [(CircuitInterfacePath, BV)]
               , exposedPath :: CircuitInterfacePath }
        deriving Show

bvBitWidth :: BV -> Maybe BitWidth
bvBitWidth bv = queryCircuitInterfaceAt getPortOutBitWidth ifc bv.exposedPath
  where ifc = circuitInterface . primitiveInfo $ bv.primitive

unsafeBVBitWidth :: BV -> BitWidth
unsafeBVBitWidth bv = fromMaybe err $ bvBitWidth bv
  where err = error $ "could not extract BitWidth for "
                      ++ show bv.exposedPath ++ " in " ++ show ifc
        ifc = circuitInterface . primitiveInfo $ bv.primitive

{-# NOINLINE instanceIdCnt #-}
-- | Global 'InstanceId' counter
instanceIdCnt :: IORef InstanceId
instanceIdCnt = unsafePerformIO $ newIORef 0

{-# NOINLINE mkPrimitive #-}
-- | Helper function for creating an instance of a primitive component
mkPrimitive :: Primitive -> [(CircuitInterfacePath, BV)] -> CircuitInterface
            -> [BV]
mkPrimitive prim rcvSigs ifc = case getPortOuts ifc of
  [] -> [bv]
  ports -> [ bv { exposedPath = path } | (path, _) <- ports ]
  where -- | model BV
        bv = MkBV { instanceId = iId
                  , primitive = prim
                  , receivedSignals = rcvSigs
                  , exposedPath = mempty }
        -- | For Observable Sharing.
        iId = unsafePerformIO $ atomicModifyIORef' instanceIdCnt \x -> (x+1, x)

mkUnOpBV :: Primitive -> BV -> BV
mkUnOpBV prim x = head $ mkPrimitive prim [([0], x)]
                                          (ifcUnaryOp $ unsafeBVBitWidth x)

mkBinOpBV :: Primitive -> BV -> BV -> BV
mkBinOpBV prim x y = head $ mkPrimitive prim [ ([0], x), ([1], y) ]
                                             (ifcBinaryOp $ unsafeBVBitWidth x)

mkConstantBV :: Integer -> BitWidth -> BV
mkConstantBV v w = head $ mkPrimitive (Constant v w) [] (ifc0in1out w)

mkAndBV :: BV -> BV -> BV
mkAndBV x y = mkBinOpBV (And $ unsafeBVBitWidth x) x y

mkOrBV :: BV -> BV -> BV
mkOrBV x y = mkBinOpBV (Or $ unsafeBVBitWidth x) x y

mkCustomBV :: Primitive -> [(CircuitInterfacePath, BV)] -> [BV]
mkCustomBV p@Custom{..} rcvSigs = mkPrimitive p rcvSigs interface

mkInterfaceBV :: CircuitInterface -> [BV]
mkInterfaceBV ifc = mkPrimitive (Interface ifc) [] ifc

--------------------------------------------------------------------------------
-- Net for full-blown graph style netlist

data NetPort = NetPort InstanceId CircuitInterfacePath
             | NetPortInlined Primitive [NetPort]
             deriving Show

data Net = MkNet { instanceId :: InstanceId
                 , primitive :: Primitive
                 , inputPorts :: [NetPort] }
         deriving Show

-- flattening of the untyped bitvector representation to the netlist graph
-- representation

type FlattenBV = StateT FlattenS (WriterT FlattenW Identity)

type FlattenS = IntSet.IntSet

type FlattenW = [Net]

execFlattenBV :: FlattenBV a -> FlattenS -> (FlattenS, FlattenW, a)
execFlattenBV m s0 = (s, w, x)
  where f = runIdentity . runWriterT . (flip runStateT) s0
        ((x, s), w) = f m

getVisited :: FlattenBV FlattenS
getVisited = get

putVisited :: FlattenS -> FlattenBV ()
putVisited = put

addNet :: Net -> FlattenBV ()
addNet = lift . tell . (:[])

flattenBV :: BV -> FlattenBV NetPort
flattenBV MkBV{ primitive = p@(Constant _ _) } = return $ NetPortInlined p []
flattenBV bv = do
  visited <- getVisited
  when (not $ bv.instanceId `IntSet.member` visited) do
    putVisited $ IntSet.insert bv.instanceId visited
    inPorts <- mapM flattenBV (snd <$> bv.receivedSignals)
    addNet MkNet { instanceId = bv.instanceId
                 , primitive  = bv.primitive
                 , inputPorts = inPorts }
  return $ NetPort bv.instanceId bv.exposedPath

flattenBV_ :: BV -> FlattenBV ()
flattenBV_ bv = flattenBV bv >> return ()

--------------------------------------------------------------------------------
-- generate circuit interface for a blarney circuit

class BuildCircuitIfc a where
  buildCircuitIfc :: Int -> a -> CircuitInterface

instance BuildCircuitIfc BV where
  buildCircuitIfc _ bv = PortOut "retVal" 8

instance BuildCircuitIfc b => BuildCircuitIfc (BV -> b) where
  buildCircuitIfc n f =
    PortIn ("arg" ++ show n) 4 <> (buildCircuitIfc (n + 1) $ f undefined)

getCircuitInterface :: BuildCircuitIfc a => a -> CircuitInterface
getCircuitInterface = buildCircuitIfc 0

--------------------------------------------------------------------------------
-- apply circuit function to get root BVs for a blarney circuit

class GetCircuitRoots b where
  getCircuitRoots :: b -> [BV] -> [BV]

instance GetCircuitRoots BV where
  getCircuitRoots bv [] = [bv]

instance GetCircuitRoots b => GetCircuitRoots (BV -> b) where
  getCircuitRoots f (bv : bvs) = getCircuitRoots (f bv) bvs

--------------------------------------------------------------------------------
-- produce Circuit Netlist and Interface

data Circuit = Circuit { interface :: CircuitInterface
                       , netlist   :: [Net] }
instance Show Circuit where
  show Circuit{..} = render cDoc
    where cDoc =     text "- interface:" $$ text (show interface)
                 $+$ text "- netlist:" $+$ vcat (text <$> (show <$> netlist))

buildCircuit :: (BuildCircuitIfc a, GetCircuitRoots a) => a -> Circuit
buildCircuit f = Circuit { interface = ifc, netlist = nl }
  where ifc = getCircuitInterface f
        ifcBVs = mkInterfaceBV $ flipCircuitInterface ifc
        rootBVs = getCircuitRoots f ifcBVs
        (_, nl, _) = execFlattenBV (mapM flattenBV_ rootBVs) mempty

--------------------------------------------------------------------------------
-- generate some verilog

writeVerilog :: String -> Circuit -> String
writeVerilog nm c = nm ++ "\n" ++ show c ++ "\n- no verilog yet"
