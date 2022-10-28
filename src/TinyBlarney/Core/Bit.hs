{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |

Module      : TinyBlarney.Core.Bit
Description : TinyBlarney's sized bit vector representation
Stability   : experimental

This module defines the 'Bit n' type and a set of primitive operations on that
type.

-}

module TinyBlarney.Core.Bit (
  Bit (..)
, valueOf
, unsafeWidthOf
, unsafeBitConstant
, bitConstant
, bitZero
, unsafeBitZeros
, bitZeros
, bitOne
, unsafeBitOnes
, bitOnes
, bitDontCare
, unsafeBitDontCares
, bitDontCares
, bitAnd
, bitOr
, bitXor
, bitInvert
, bitConcat
, unsafeBitSlice
, unsafeFromBitList
, unsafeToBitList
) where

import TinyBlarney.Misc.Misc
import TinyBlarney.Core.BV

import GHC.TypeLits

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.Bit: " ++ m

-- | Type representing a sized bit vector
newtype Bit (n :: Nat) = AsBit { bv :: BV }

-- | Get the size of a 'Bit n' without relying on its type
unsafeWidthOf :: Bit n -> Int
unsafeWidthOf = unsafeBitWidthBV . bv

-- | Constant of an explicitly user-provided width
unsafeBitConstant :: Integer -> Int -> Bit n
unsafeBitConstant k w = AsBit $ mkConstantBV k w

-- | Constant of a given value
bitConstant :: forall n. KnownNat n => Integer -> Bit n
bitConstant k = unsafeBitConstant k (valueOf @n)

-- | A single bit 0
bitZero :: Bit 1
bitZero = bitConstant 0

-- | All bits zero with explicitly user-provided width
unsafeBitZeros :: Int -> Bit n
unsafeBitZeros w = unsafeFromBitList $ replicate w bitZero

-- | All bits zero
bitZeros :: forall n. KnownNat n => Bit n
bitZeros = unsafeBitZeros (valueOf @n)

-- | A single bit 1
bitOne :: Bit 1
bitOne = bitConstant 1

-- | All bits one with explicitly user-provided width
unsafeBitOnes :: Int -> Bit n
unsafeBitOnes w = unsafeFromBitList $ replicate w bitOne

-- | All bits one
bitOnes :: forall n. KnownNat n => Bit n
bitOnes = unsafeBitOnes (valueOf @n)

-- |A single bit don't care
bitDontCare :: Bit 1
bitDontCare = AsBit $ mkDontCareBV 1

-- | All bits don't care with explicitly user-provided width
unsafeBitDontCares :: Int -> Bit n
unsafeBitDontCares w = unsafeFromBitList $ replicate w bitDontCare

-- | All bits don't care
bitDontCares :: forall n. KnownNat n => Bit n
bitDontCares = unsafeBitDontCares (valueOf @n)

-- | @bitAnd x y@ returns the bitwise "and" of @x@ and @y@
bitAnd :: Bit n -> Bit n -> Bit n
bitAnd x y = AsBit $ mkAndBV x.bv y.bv

-- | @bitOr x y@ returns the bitwise "or" of @x@ and @y@
bitOr :: Bit n -> Bit n -> Bit n
bitOr x y = AsBit $ mkOrBV x.bv y.bv

-- | @bitXor x y@ returns the bitwise "xor" of @x@ and @y@
bitXor :: Bit n -> Bit n -> Bit n
bitXor x y = AsBit $ mkXorBV x.bv y.bv

-- | @bitInvert x@ returns the ones' complement of @x@
bitInvert :: Bit n -> Bit n
bitInvert x = AsBit $ mkInvertBV x.bv

-- | @bitConcat x y@ returns the concatenation of @x@ above @y@
bitConcat :: Bit n -> Bit m -> Bit (n+m)
bitConcat x y = AsBit $ mkConcatBV x.bv y.bv

-- | @unsafeBitSlice (hi, lo) x@ returns the slice of @(hi-lo+1)@-bit wide slice
--   of @x@ between bit indices @hi@ and @lo@ (both included)
unsafeBitSlice :: forall n m. (Int, Int) -> Bit n -> Bit m
unsafeBitSlice (hi, lo) x
  | hi >= lo && wOut <= wIn = AsBit $ mkSliceBV (hi, lo) x.bv
  | otherwise = err $ "unsafeBitSlice: unmet constraints, "
                      ++ "hi: " ++ show hi
                      ++ "lo: " ++ show lo
                      ++ "wIn: " ++ show wIn
                      ++ "wOut: " ++ show wOut
  where wOut = hi-lo+1
        wIn = unsafeWidthOf x

-- | Collapse a '[Bit 1]' bit list of size n to a 'Bit n' sized bit vector
unsafeFromBitList :: [Bit 1] -> Bit n
unsafeFromBitList [] = AsBit $ mkDontCareBV 0
unsafeFromBitList xs =
  AsBit . foldr1 mkConcatBV . reverse . fmap (\x -> x.bv) $ xs

-- | Expand a sized bit vector 'Bit n' to a '[Bit 1]' bit list of size n
unsafeToBitList :: Bit n -> [Bit 1]
unsafeToBitList x = [unsafeBitSlice (i, i) x | i <- [0..unsafeWidthOf x - 1]]
