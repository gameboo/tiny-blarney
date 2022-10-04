{-# OPTIONS_HADDOCK prune #-}

{- |

Module      : TinyBlarney.Core
Description : TinyBlarney's internal "core" module
Stability   : experimental

This TinyBlarney core module contains the internals of the library. It exports
a subset of relevant functionalities.

-}

module TinyBlarney.Core (
  module TinyBlarney.Core.Bit
, Bits (SizeOf, sizeOf, pack, unpack)
, module TinyBlarney.Core.BuildCircuit
, module TinyBlarney.Core.CircuitInterface
, module TinyBlarney.Core.NetPrimitives
, module TinyBlarney.Core.NetHelpers
) where

import TinyBlarney.Core.Bit
import TinyBlarney.Core.Bits
import TinyBlarney.Core.BuildCircuit
import TinyBlarney.Core.CircuitInterface
import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.NetHelpers
