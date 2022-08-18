{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TinyBlarney.Core.Bit (
  Bit (..)
, valueOf
, widthOf
, unsafeWidthOf
, (.&.)
, (.|.)
) where

import TinyBlarney.Core.BV

import GHC.TypeLits
import Data.Proxy

newtype Bit (n :: Nat) = AsBit { bv :: BV }

-- | Convert type Nat to Ingeter value
valueOf :: forall n. (KnownNat n) => Int
valueOf = fromInteger (natVal @n Proxy)

-- | Determine width of bit-vector from type
widthOf :: forall n. KnownNat n => Bit n -> Int
widthOf _ = valueOf @n

-- | Determine width of bit-vector from underlying 'BV'
unsafeWidthOf :: Bit n -> Int
unsafeWidthOf = unsafeBVBitWidth . bv

-- | Bitwise and
infixl 7 .&.
(.&.) :: Bit n -> Bit n -> Bit n
a .&. b = AsBit $ mkAndBV a.bv b.bv

-- | Bitwise or
infixl 5 .|.
(.|.) :: Bit n -> Bit n -> Bit n
a .|. b = AsBit $ mkOrBV a.bv b.bv
