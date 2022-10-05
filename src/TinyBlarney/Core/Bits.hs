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

import TinyBlarney.Core.BV
import TinyBlarney.Core.Bit
import TinyBlarney.Core.Misc
import TinyBlarney.Core.CircuitInterface

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

  -- | Craft a value of type 'a' in 'Bits' from a '[BV]'
  fromBVs :: [BV] -> a
  default fromBVs :: GBits a => [BV] -> a
  fromBVs = to . fromBVs'

class Bits' f where
  type SizeOf' f :: Nat
  sizeOf' :: f p -> Int
  pack' :: f p -> Bit (SizeOf' f)
  unpack' :: Bit (SizeOf' f) -> f p
  -- the Int argument is the field's index in the current level of :*: chain
  getExternalInterface' :: Int -> f p -> CircuitInterface
  toBVs' :: f p -> [BV]
  fromBVs' :: [BV] -> f p

instance Bits' U1 where
  type SizeOf' U1 = 0
  sizeOf' _ = 0
  pack' _ = bitZeros
  unpack' _ = U1
  getExternalInterface' _ _ = mempty
  toBVs' _ = []
  fromBVs' _ = U1

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
  fromBVs' bvs = x :*: y
    where x = fromBVs' xBVs
          y = fromBVs' yBVs
          (xBVs, yBVs) = widthSplit wx wy bvs
          wx = sizeOf' x
          wy = sizeOf' y

instance (Bits c) => Bits' (K1 i c) where
  type SizeOf' (K1 i c) = SizeOf c
  sizeOf' ~(K1 x) = sizeOf x
  pack' ~(K1 x) = pack x
  unpack' = K1 . unpack
  getExternalInterface' _ ~(K1 x) = getExternalInterface Nothing x
  toBVs' ~(K1 x) = toBVs x
  fromBVs' = K1 . fromBVs

instance (Bits' f, Selector t) => Bits' (M1 S t f) where
  type SizeOf' (M1 S t f) = SizeOf' f
  sizeOf' ~(M1 x) = sizeOf' x
  pack' ~(M1 x) = pack' x
  unpack' = M1 . unpack'
  getExternalInterface' n m@(~(M1 x))
    | null $ selName m = metaNameHint "tpl" $ getExternalInterface' n x
    | otherwise = metaNameHint (selName m) (getExternalInterface' n x)
  toBVs' ~(M1 x) = toBVs' x
  fromBVs' = M1 . fromBVs'

instance {-# OVERLAPPABLE #-} (Bits' f) => Bits' (M1 i t f) where
  type SizeOf' (M1 i t f) = SizeOf' f
  sizeOf' ~(M1 x) = sizeOf' x
  pack' ~(M1 x) = pack' x
  unpack' = M1 . unpack'
  getExternalInterface' n ~(M1 x) = getExternalInterface' n x
  toBVs' ~(M1 x) = toBVs' x
  fromBVs' = M1 . fromBVs'

-- Standard Bits instances

instance KnownNat n => Bits (Bit n) where
  type SizeOf (Bit n) = n
  sizeOf _ = valueOf @n
  pack = id
  unpack = id
  getExternalInterface (Just name) _ = metaNameHint name $ Port Out (valueOf @n)
  getExternalInterface Nothing _ = Port Out $ valueOf @n
  toBVs x = [x.bv]
  fromBVs [bv] | unsafeBVBitWidth bv == valueOf @n = AsBit bv
  fromBVs bvs = err $ "malformed input in fromBVs: " ++ show bvs

instance Bits ()
instance (Bits a, Bits b) => Bits (a, b)
instance (Bits a, Bits b, Bits c) => Bits (a, b, c)

--------------------------------------------------------------------------------
-- local helpers

widthList :: [BV] -> BitWidth
widthList [] = 0
widthList (bv:bvs) = unsafeBVBitWidth bv + widthList bvs

widthSplit :: BitWidth -> BitWidth -> [BV] -> ([BV], [BV])
widthSplit ltgt rtgt allBVs = go 0 [] allBVs
  where go n acc (bv : bvs)
          | n + unsafeBVBitWidth bv == ltgt && widthList bvs == rtgt =
            ((reverse $ bv:acc), bvs)
          | newN <- n + unsafeBVBitWidth bv, newN < ltgt = go newN (bv:acc) bvs
          | otherwise = err $    "malformed input in widthSplit:"
                              ++ "\n  ltgt: " ++ show ltgt
                              ++ "\n  rtgt: " ++ show rtgt
                              ++ "\n  allBVs: " ++ show allBVs
