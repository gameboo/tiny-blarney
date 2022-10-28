{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- |

Module      : TinyBlarney.Misc.Misc
Description : TinyBlarney's miscelaneous utilities
Stability   : experimental

These functions do not depend on anything specific to TinyBlarney

-}

module TinyBlarney.Misc.Misc (
  valueOf
, clamp
, ceilDiv
, integerToWord8List
, word8ListToInteger
, sizedListToInteger
, unfoldWhileM
, update
, transposePropagate
) where

import Data.Bits
import Data.Word
import Data.List
import Data.Proxy
import GHC.TypeLits

-- | Lower a type of kind 'Nat' to 'Int' value (with type application @\@@)
valueOf :: forall n. (KnownNat n) => Int
valueOf = fromInteger (natVal @n Proxy)

-- | clamp a given value to the provided width
clamp :: (Num a, Bits a) => Int -> a -> a
clamp w x = (bit w - 1) .&. x

ceilDiv :: Integral a => a -> a -> a
ceilDiv x y | x `mod` y == 0 = divRes
            | otherwise = 1 + divRes
  where divRes = x `div` y

integerToWord8List :: Integer -> [Word8]
integerToWord8List 0 = []
integerToWord8List x =
  fromInteger (x `mod` 256) : integerToWord8List (x `div` 256)

word8ListToInteger :: [Word8] -> Integer
word8ListToInteger words = sum [ toInteger x * 2 ^ (8 * i)
                               | (x, i) <- zip words [0..] ]

sizedListToInteger :: (Integral a, Integral b) => [(a, b)] -> Integer
sizedListToInteger = snd . foldl f (0, 0)
  where f (accW, accN) (w, n) = (accW + w, accN + (toInteger n * 2 ^ accW))

unfoldWhileM :: Monad m => (a -> Bool) -> m a -> m [a]
unfoldWhileM p m =
  do x <- m
     if not (p x) then return [x] else (x:) <$> unfoldWhileM p m

update :: [t] -> Int -> t -> [t]
update xs i x | i < 0 || i > length xs - 1 = xs
              | otherwise = take i xs ++ (x : drop (i+1) xs)

transposePropagate :: forall a b. (Ord a, Bounded a)
                   => [b] -> [[(a, b)]] -> [(a, [b])]
transposePropagate zeroes ss = go zeroes (sortBy ordFun . tagWithIdx $ ss)
 where tagWithIdx :: [[t]] -> [(Int, [t])]
       tagWithIdx = go 0
         where go _ [] = []
               go n (xs:xss) = (n, xs): go (n+1) xss
       ordField :: (Int, [(a, b)]) -> a
       ordField (_, []) = maxBound
       ordField (_, (x,_):_) = x
       ordFun :: (Int, [(a, b)]) -> (Int, [(a, b)]) -> Ordering
       ordFun x y = compare (ordField x) (ordField y)
       --
       go :: [b] -> [(Int, [(a, b)])] -> [(a, [b])]
       go _ [] = []
       go s ((_, []):xyzs) = go s xyzs
       go s ((i, (t, w):ws):xyzs) =
         let s' = update s i w
         in (t, s') : go s' (insertBy ordFun (i, ws) xyzs)
