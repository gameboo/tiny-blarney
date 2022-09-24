{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

{- |

Module      : TinyBlarney.Core.Misc
Description : Various helpers not necessarily specific to TinyBlarney
Stability   : experimental

-}

module TinyBlarney.Core.Misc (
  valueOf
, If
, Max
) where

import Data.Proxy
import GHC.TypeLits
import GHC.TypeNats
import Data.Type.Equality

-- | Lower a type of kind 'Nat' to 'Int' value (with type application @\@@)
valueOf :: forall n. (KnownNat n) => Int
valueOf = fromInteger (GHC.TypeLits.natVal @n Proxy)

type If :: Bool -> k -> k -> k
type family If cond thn els where
  If 'True  x _ = x
  If 'False _ x = x

type Max :: Nat -> Nat -> Nat
type family Max n0 n1 where
  Max x y = If (CmpNat x y == 'GT) x y
