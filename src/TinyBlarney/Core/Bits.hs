{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

{- |

Module      : TinyBlarney.Core.Bits
Description : TinyBlarney's 'Bits' class for types with a 'Bit n' representation
Stability   : experimental

-}

module TinyBlarney.Core.Bits (
  Bits (..)
, externalInterface
, internalInterface
) where

import TinyBlarney.Misc.Misc
import TinyBlarney.Core.BV
import TinyBlarney.Core.Bit
import TinyBlarney.Core.BasicTypes

import GHC.TypeLits
import GHC.Generics

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.Bits: " ++ m

-- | The "external" CircuitInterface for the type 'a' in 'Bits'
externalInterface :: forall a. Bits a => Maybe String -> CircuitInterface
externalInterface nm = getExternalInterface nm dummy
  where dummy :: a
        dummy = unpack $ unsafeBitDontCares (sizeOf (undefined :: a))

-- | The "internal" CircuitInterface for the type 'a' in 'Bits'
internalInterface :: forall a. Bits a => Maybe String -> CircuitInterface
internalInterface nm = flipCircuitInterface $ externalInterface @a nm

-- type synonym for repeated constraint
type GBits a = (Generic a, Bits' (Rep a), SizeOf' (Rep a) ~ SizeOf a)
class Bits a where

  -- | Type level bit vector size of the type in 'Bits'
  type SizeOf a :: Nat
  type SizeOf a = SizeOf' (Rep a)

  -- | Value level bit vector size of a value of the type in 'Bits'
  sizeOf :: a -> Int
  default sizeOf :: GBits a => a -> Int
  sizeOf = sizeOf' . from

  -- | Convert to a bit vector
  pack :: a -> Bit (SizeOf a)
  default pack :: GBits a => a -> Bit (SizeOf a)
  pack = pack' . from

  -- | Convert from a bit vector
  unpack :: Bit (SizeOf a) -> a
  default unpack :: GBits a => Bit (SizeOf a) -> a
  unpack = to . unpack'

  -- | Get corresponding external port as a CircuitInterface
  getExternalInterface :: Maybe String -> a -> CircuitInterface
  default getExternalInterface :: GBits a
                               => Maybe String -> a -> CircuitInterface
  getExternalInterface (Just name) =
    metaNameHint name . getExternalInterface' 0 . from
  getExternalInterface Nothing = getExternalInterface' 0 . from

  -- | Retrieve 'BV's from a type in 'Bits'
  toBVs :: a -> [BV]
  default toBVs :: GBits a => a -> [BV]
  toBVs = toBVs' . from

  -- | Parse a value of type 'a' in 'Bits' from a '[BV]', and returns a triple
  --   with the value parsed first, the number of 'BV's consumed from the input
  --   list second, and the new remaining list third
  fromBVs :: [BV] -> (a, Int, [BV])
  default fromBVs :: GBits a => [BV] -> (a, Int, [BV])
  fromBVs = (\(x, y, z) -> (to x, y, z)) . fromBVs'

class Bits' f where
  type SizeOf' f :: Nat
  sizeOf' :: f p -> Int
  pack' :: f p -> Bit (SizeOf' f)
  unpack' :: Bit (SizeOf' f) -> f p
  -- the Int argument is the field's index in the current level of :*: chain
  getExternalInterface' :: Int -> f p -> CircuitInterface
  toBVs' :: f p -> [BV]
  fromBVs' :: [BV] -> (f p, Int, [BV])

instance Bits' U1 where
  type SizeOf' U1 = 0
  sizeOf' _ = 0
  pack' _ = bitZeros
  unpack' _ = U1
  getExternalInterface' _ _ = mempty
  toBVs' _ = []
  fromBVs' bvs = (U1, 0, bvs)

-- No instance for Bits' (f :+: g)

instance (Bits' f, Bits' g) => Bits' (f :*: g) where
  type SizeOf' (f :*: g) = SizeOf' f + SizeOf' g
  sizeOf' ~(x :*: y) = sizeOf' x + sizeOf' y
  pack' ~(x :*: y) = bitConcat (pack' x)  (pack' y)
  unpack' b = x :*: y
    where x = unpack' $ unsafeBitSlice (wx + wy - 1, wy) b
          y = unpack' $ unsafeBitSlice (wy - 1, 0) b
          wx = sizeOf' x
          wy = sizeOf' y
  getExternalInterface' n ~(x :*: y) =
    getExternalInterface' n x <> getExternalInterface' (n+1) y
  toBVs' ~(x :*: y) = toBVs' x ++ toBVs' y
  fromBVs' bvs = (x :*: y, nx + ny, bvs'')
    where (x, nx, bvs') = fromBVs' bvs
          (y, ny, bvs'') = fromBVs' bvs'

instance (Bits c) => Bits' (K1 i c) where
  type SizeOf' (K1 i c) = SizeOf c
  sizeOf' ~(K1 x) = sizeOf x
  pack' ~(K1 x) = pack x
  unpack' = K1 . unpack
  getExternalInterface' _ ~(K1 x) = getExternalInterface Nothing x
  toBVs' ~(K1 x) = toBVs x
  fromBVs' = (\(x, n, bvs) -> (K1 x, n, bvs)) . fromBVs

instance (Bits' f, Selector t) => Bits' (M1 S t f) where
  type SizeOf' (M1 S t f) = SizeOf' f
  sizeOf' ~(M1 x) = sizeOf' x
  pack' ~(M1 x) = pack' x
  unpack' = M1 . unpack'
  getExternalInterface' n m@(~(M1 x))
    | null $ selName m = metaNameHint "tpl" $ getExternalInterface' n x
    | otherwise = metaNameHint (selName m) (getExternalInterface' n x)
  toBVs' ~(M1 x) = toBVs' x
  fromBVs' = (\(x, n, bvs) -> (M1 x, n, bvs)) . fromBVs'

instance {-# OVERLAPPABLE #-} (Bits' f) => Bits' (M1 i t f) where
  type SizeOf' (M1 i t f) = SizeOf' f
  sizeOf' ~(M1 x) = sizeOf' x
  pack' ~(M1 x) = pack' x
  unpack' = M1 . unpack'
  getExternalInterface' n ~(M1 x) = getExternalInterface' n x
  toBVs' ~(M1 x) = toBVs' x
  fromBVs' = (\(x, n, bvs) -> (M1 x, n, bvs)) . fromBVs'

-- Standard Bits instances

instance KnownNat n => Bits (Bit n) where
  type SizeOf (Bit n) = n
  sizeOf _ = valueOf @n
  pack = id
  unpack = id
  getExternalInterface (Just name) _ = metaNameHint name $ Port Out (valueOf @n)
  getExternalInterface Nothing _ = Port Out $ valueOf @n
  toBVs x = [x.bv]
  fromBVs (bv:bvs) | unsafeBitWidthBV bv == valueOf @n = (AsBit bv, 1, bvs)

instance Bits ()
instance (Bits a, Bits b) => Bits (a, b)
instance (Bits a, Bits b, Bits c) => Bits (a, b, c)
