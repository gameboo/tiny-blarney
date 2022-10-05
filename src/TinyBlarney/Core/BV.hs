{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module TinyBlarney.Core.BV (
  BV (..)
, prettyBV
, PathAndBV
, bvBitWidth
, unsafeBVBitWidth
, mkConstantBV
, mkDontCareBV
, mkAndBV
, mkOrBV
, mkXorBV
, mkInvertBV
, mkConcatBV
, mkSliceBV
, mkCustomBV
, mkInterfaceBV
) where

import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

import Data.Maybe
import Prelude hiding ((<>))
import Text.PrettyPrint

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.BV: " ++ m

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

-- | Pretty print a 'BV'.
prettyBV :: BV -> Doc
prettyBV MkBV{..} =
  text "bv" <> int instanceId <> prettyCircuitInterfacePath exposedPath
            <+> sep xs
  where
    xs = [ primPretty primitive
         , case receivedSignals of
             [] -> text "No Inputs"
             _ -> text "Inputs" <+> braces (nest 2 $ commaSep rcvPathsDocs) ]
    rcvPathsDocs = prettyCircuitInterfacePath . fst <$> receivedSignals
    commaSep = sep . punctuate comma

-- | Show instance for 'BV'.
instance Show BV where
  show = render . prettyBV

type PathAndBV = (CircuitInterfacePath, BV)

bvBitWidth :: BV -> Maybe BitWidth
bvBitWidth bv = queryCircuitInterfaceAt getPortOutBitWidth ifc bv.exposedPath
  where ifc = primInterface bv.primitive

unsafeBVBitWidth :: BV -> BitWidth
unsafeBVBitWidth bv = fromMaybe failure $ bvBitWidth bv
  where failure = err $ "could not extract BitWidth for "
                        ++ show bv.exposedPath ++ " in " ++ show ifc
        ifc = primInterface bv.primitive

{-# NOINLINE instanceIdCnt #-}
-- | Global 'InstanceId' counter
instanceIdCnt :: IORef InstanceId
instanceIdCnt = unsafePerformIO $ newIORef 0

{-# NOINLINE mkPrimitive #-}
-- | Helper function for creating an instance of a primitive component
mkPrimitive :: Primitive -> [PathAndBV] -> [BV]
mkPrimitive prim rcvSigs = case getPortOutPaths . primInterface $ prim of
  [] -> [bv]
  paths -> [ bv { exposedPath = path } | path <- paths ]
  where -- | model BV
        bv = MkBV { instanceId = iId
                  , primitive = prim
                  , receivedSignals = rcvSigs
                  , exposedPath = mempty }
        -- | For Observable Sharing.
        iId = unsafePerformIO $ atomicModifyIORef' instanceIdCnt \x -> (x+1, x)

mkUnOpBV :: Primitive -> BV -> BV
mkUnOpBV prim x = head $ mkPrimitive prim (zip (primInputPaths prim) [x])

mkBinOpBV :: Primitive -> BV -> BV -> BV
mkBinOpBV prim x y = head $ mkPrimitive prim (zip (primInputPaths prim) [x, y])

mkConstantBV :: Integer -> BitWidth -> BV
mkConstantBV v w = head $ mkPrimitive (Constant v w) []

mkDontCareBV :: BitWidth -> BV
mkDontCareBV w = head $ mkPrimitive (DontCare w) []

mkAndBV :: BV -> BV -> BV
mkAndBV x y = mkBinOpBV (And $ unsafeBVBitWidth x) x y

mkOrBV :: BV -> BV -> BV
mkOrBV x y = mkBinOpBV (Or $ unsafeBVBitWidth x) x y

mkXorBV :: BV -> BV -> BV
mkXorBV x y = mkBinOpBV (Xor $ unsafeBVBitWidth x) x y

mkInvertBV :: BV -> BV
mkInvertBV x = mkUnOpBV (Invert $ unsafeBVBitWidth x) x

mkConcatBV :: BV -> BV -> BV
mkConcatBV x y = mkBinOpBV (Concatenate wx wy) x y
  where wx = unsafeBVBitWidth x
        wy = unsafeBVBitWidth y

mkSliceBV :: (Int, Int) -> BV -> BV
mkSliceBV (hi, lo) x = mkUnOpBV (Slice (hi, lo) $ unsafeBVBitWidth x) x

mkCustomBV :: Circuit -> [PathAndBV] -> [BV]
mkCustomBV circuit rcvSigs = mkPrimitive (Custom circuit) rcvSigs

mkInterfaceBV :: CircuitInterface -> [PathAndBV] -> [BV]
mkInterfaceBV ifc rcvSigs = mkPrimitive (Interface ifc) rcvSigs
