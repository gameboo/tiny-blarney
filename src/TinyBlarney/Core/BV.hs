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
, bitWidthBV
, unsafeBitWidthBV
, mkConstantBV
, mkDontCareBV
, mkAndBV
, mkOrBV
, mkXorBV
, mkInvertBV
, mkConcatBV
, mkSliceBV
, mkMergeBV
, mkCustomBV
, mkInterfaceBV
, evaluateBV
, unsafeEvaluateBV
) where

import TinyBlarney.Core.BasicTypes

import Data.Maybe
import Control.Monad
import Text.PrettyPrint
import Prelude hiding ((<>))

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

data BV = BV { instanceId :: InstanceId
             , primitive :: Primitive
             , receivedSignals :: [PathAndBV]
             , exposedPath :: CircuitInterfacePath }

-- | Pretty print a 'BV'.
prettyBV :: BV -> Doc
prettyBV BV{..} =
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

bitWidthBV :: BV -> Maybe BitWidth
bitWidthBV bv = queryCircuitInterfaceAt getPortOutBitWidth ifc bv.exposedPath
  where ifc = primInterface bv.primitive

unsafeBitWidthBV :: BV -> BitWidth
unsafeBitWidthBV bv = fromMaybe failure $ bitWidthBV bv
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
mkPrimitive prim rcvSigs =
  case getExplicitPortOutPaths . primInterface $ prim of
    [] -> [bv]
    paths -> [ bv { exposedPath = path } | path <- paths ]
  where -- | model BV
        bv = BV { instanceId = iId
                , primitive = prim
                , receivedSignals = rcvSigs
                , exposedPath = mempty }
        -- | For Observable Sharing.
        iId = unsafePerformIO $ atomicModifyIORef' instanceIdCnt \x -> (x+1, x)

mkPrimitiveWithPaths :: Primitive -> [BV] -> [BV]
mkPrimitiveWithPaths prim xs = mkPrimitive prim (zip (primInputPaths prim) xs)

mkUnOpBV :: Primitive -> BV -> BV
mkUnOpBV prim x = head $ mkPrimitiveWithPaths prim [x]

mkBinOpBV :: Primitive -> BV -> BV -> BV
mkBinOpBV prim x y = head $ mkPrimitiveWithPaths prim [x, y]

mkConstantBV :: Integer -> BitWidth -> BV
mkConstantBV v w = head $ mkPrimitive (Constant v w) []

mkDontCareBV :: BitWidth -> BV
mkDontCareBV w = head $ mkPrimitive (DontCare w) []

mkAndBV :: BV -> BV -> BV
mkAndBV x y = mkBinOpBV (And $ unsafeBitWidthBV x) x y

mkOrBV :: BV -> BV -> BV
mkOrBV x y = mkBinOpBV (Or $ unsafeBitWidthBV x) x y

mkXorBV :: BV -> BV -> BV
mkXorBV x y = mkBinOpBV (Xor $ unsafeBitWidthBV x) x y

mkInvertBV :: BV -> BV
mkInvertBV x = mkUnOpBV (Invert $ unsafeBitWidthBV x) x

mkConcatBV :: BV -> BV -> BV
mkConcatBV x y = mkBinOpBV (Concatenate wx wy) x y
  where wx = unsafeBitWidthBV x
        wy = unsafeBitWidthBV y

mkSliceBV :: (Int, Int) -> BV -> BV
mkSliceBV (hi, lo) x = mkUnOpBV (Slice (hi, lo) $ unsafeBitWidthBV x) x

mkMergeBV :: MergeStrategy -> [(BV, BV)] -> BV
mkMergeBV mStrat ins = head $ mkPrimitiveWithPaths (Merge mStrat n w) ins'
  where n = length ins
        w = if n > 0 then unsafeBitWidthBV (snd . head $ ins) else 0
        -- XXX could check that all fst elemens are size one and all snd elems
        -- are same size w
        ins' = concatMap (\(en, x) -> [en, x]) ins

mkCustomBV :: Circuit -> [PathAndBV] -> [BV]
mkCustomBV circuit rcvSigs = mkPrimitive (Custom circuit) rcvSigs

mkInterfaceBV :: CircuitInterface -> [PathAndBV] -> [BV]
mkInterfaceBV ifc rcvSigs = mkPrimitive (Interface ifc) rcvSigs

evaluateBV :: BV -> Maybe Integer
evaluateBV bv = do let (ps, xs) = unzip bv.receivedSignals
                   xs' <- forM xs evaluateBV
                   primEvalFirst bv.primitive (zip ps xs')

unsafeEvaluateBV :: BV -> Integer
unsafeEvaluateBV bv = fromMaybe fail $ evaluateBV bv
  where fail = err $ "cannot evaluate " ++ show bv
