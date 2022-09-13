{-# OPTIONS_HADDOCK prune #-}

{- |

Module      : TinyBlarney
Description : Hardware description in Haskell
Stability   : experimental

This is the top-level of the TinyBlarney library. It exports a set of internal
TinyBlarney functionalities.

-}

module TinyBlarney (
  module TinyBlarney.Core
, module TinyBlarney.Backends
) where

import TinyBlarney.Core
import TinyBlarney.Backends
