{-# OPTIONS_HADDOCK prune #-}

{- |

Module      : TinyBlarney.Backends
Description : Available backends for TinyBlarney
Stability   : experimental

This is the set of TinyBlarney backends.

-}

module TinyBlarney.Backends (
  module TinyBlarney.Backends.CodeGeneration.SMT
, module TinyBlarney.Backends.CodeGeneration.Verilog
, module TinyBlarney.Backends.Simulation.Haskell
, module TinyBlarney.Backends.Simulation.Verilator
, module TinyBlarney.Backends.Verification.SMT
) where

import TinyBlarney.Backends.CodeGeneration.SMT
import TinyBlarney.Backends.CodeGeneration.Verilog
import TinyBlarney.Backends.Simulation.Haskell
import TinyBlarney.Backends.Simulation.Verilator
import TinyBlarney.Backends.Verification.SMT
