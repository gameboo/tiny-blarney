{-# OPTIONS_HADDOCK prune #-}

{- |

Module      : TinyBlarney.Core
Description : TinyBlarney's internal "core" module
Stability   : experimental

This TinyBlarney core module contains the internals of the library. It exports
a subset of relevant functionalities.

-}

module TinyBlarney.Core (
  Bits (SizeOf, sizeOf, pack, unpack)
, module ReExport
) where

import TinyBlarney.Core.Bit as ReExport
import TinyBlarney.Core.Bits as ReExport
import TinyBlarney.Core.BuildCircuit as ReExport
import TinyBlarney.Core.BasicTypes as ReExport
import TinyBlarney.Core.NetHelpers as ReExport
