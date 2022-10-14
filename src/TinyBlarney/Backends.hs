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
, module TinyBlarney.Backends.Simulation.SimulatorBuild
, module TinyBlarney.Backends.Verification.SMT
) where

import TinyBlarney.Backends.CodeGeneration.SMT
import TinyBlarney.Backends.CodeGeneration.Verilog
import TinyBlarney.Backends.Simulation.SimulatorBuild
import TinyBlarney.Backends.Verification.SMT
