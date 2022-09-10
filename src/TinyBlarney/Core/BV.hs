{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module TinyBlarney.Core.BV (
  BV (..)
, PathAndBV
, bvBitWidth
, unsafeBVBitWidth
, mkConstantBV
, mkAndBV
, mkOrBV
, mkCustomBV
, mkInterfaceBV
) where

import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

import Data.Maybe

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

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
               , receivedSignals :: [PathAndBV]
               , exposedPath :: CircuitInterfacePath }
        deriving Show

type PathAndBV = (CircuitInterfacePath, BV)

bvBitWidth :: BV -> Maybe BitWidth
bvBitWidth bv = queryCircuitInterfaceAt getPortOutBitWidth ifc bv.exposedPath
  where ifc = (primitiveInfo $ bv.primitive).interface

unsafeBVBitWidth :: BV -> BitWidth
unsafeBVBitWidth bv = fromMaybe err $ bvBitWidth bv
  where err = error $ "could not extract BitWidth for "
                      ++ show bv.exposedPath ++ " in " ++ show ifc
        ifc = (primitiveInfo $ bv.primitive).interface

{-# NOINLINE instanceIdCnt #-}
-- | Global 'InstanceId' counter
instanceIdCnt :: IORef InstanceId
instanceIdCnt = unsafePerformIO $ newIORef 0

{-# NOINLINE mkPrimitive #-}
-- | Helper function for creating an instance of a primitive component
mkPrimitive :: Primitive -> [PathAndBV] -> CircuitInterface
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
mkUnOpBV prim x = head $ mkPrimitive prim [(Step 0, x)]
                                          (ifcUnaryOp $ unsafeBVBitWidth x)

mkBinOpBV :: Primitive -> BV -> BV -> BV
mkBinOpBV prim x y = head $ mkPrimitive prim [ (Step 0, x)
                                             , (Step 1, y) ]
                                             (ifcBinaryOp $ unsafeBVBitWidth x)

mkConstantBV :: Integer -> BitWidth -> BV
mkConstantBV v w = head $ mkPrimitive (Constant v w) [] (PortOut "out" w)

mkAndBV :: BV -> BV -> BV
mkAndBV x y = mkBinOpBV (And $ unsafeBVBitWidth x) x y

mkOrBV :: BV -> BV -> BV
mkOrBV x y = mkBinOpBV (Or $ unsafeBVBitWidth x) x y

mkCustomBV :: Primitive -> [PathAndBV] -> [BV]
mkCustomBV p@Custom{..} rcvSigs = mkPrimitive p rcvSigs interface

mkInterfaceBV :: CircuitInterface -> [PathAndBV] -> [BV]
mkInterfaceBV ifc rcvSigs = mkPrimitive (Interface ifc) rcvSigs ifc
