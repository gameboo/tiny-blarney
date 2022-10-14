{- |

Module      : TinyBlarney.Backends.Simulation.Verilator
Description : TinyBlarney's verilator simulation
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Verilator (
  simulateWithVerilator
) where

import TinyBlarney.Core
import TinyBlarney.Backends.CodeGeneration.Verilog

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Verilator: " ++ m

-- | Simulate a 'Circuit' using verilator by first generating the verilog code
--   and the appropriate C++ wrapper to then invoke verilator
simulateWithVerilator :: Circuit -> IO ()
simulateWithVerilator = err $ "Not implemented yet"
