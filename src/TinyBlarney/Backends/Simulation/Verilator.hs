{- |

Module      : TinyBlarney.Backends.Simulation.Verilator
Description : TinyBlarney's verilator simulation
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Verilator (
  buildSimulatorWithVerilator
) where

import TinyBlarney.Core
import TinyBlarney.Backends.CodeGeneration.Verilog
import TinyBlarney.Backends.Simulation.SimulatorTypes

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Verilator: " ++ m

-- | Build a 'Simulator' for a given 'Circuit' using verilator by first
--   generating the verilog code and the appropriate C++ wrapper to then invoke
--   verilator
buildSimulatorWithVerilator :: Circuit -> Simulator
buildSimulatorWithVerilator = err $ "Not implemented yet"
