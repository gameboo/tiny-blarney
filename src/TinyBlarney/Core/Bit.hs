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
, widthOf
, unsafeWidthOf
, bitZero
, bitOne
, bitDontCare
, bitAnd
, bitOr
, bitXor
, bitInvert
, bitConcat
, unsafeBitSlice
, unsafeFromBitList
, unsafeToBitList
) where

import TinyBlarney.Core.BV
import TinyBlarney.Core.Misc

import GHC.TypeLits

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.Bit: " ++ m

-- | Type representing a sized bit vector
newtype Bit (n :: Nat) = AsBit { bv :: BV }

-- | Get the size 'n' of a 'Bit n' as an 'Int'
widthOf :: forall n. KnownNat n => Bit n -> Int
widthOf _ = valueOf @n
--
-- | Get the size of a 'Bit n' without relying on its type
unsafeWidthOf :: Bit n -> Int
unsafeWidthOf = unsafeBVBitWidth . bv

-- | A single bit 0
bitZero :: Bit 1
bitZero = AsBit $ mkConstantBV 0 1

-- | A single bit 1
bitOne :: Bit 1
bitOne = AsBit $ mkConstantBV 1 1

-- |A single bit don't care
bitDontCare :: Bit 1
bitDontCare = AsBit $ mkDontCareBV 1

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
