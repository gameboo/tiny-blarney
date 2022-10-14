{-# LANGUAGE OverloadedRecordDot  #-}

{-|

Module      : TinyBlarney.Backends.Simulation.SimulatorBuild
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.SimulatorBuild (
  buildSimulatorWith
, buildSimulator
) where

import TinyBlarney.Core
import TinyBlarney.Backends.Simulation.Haskell
import TinyBlarney.Backends.Simulation.Verilator
import TinyBlarney.Backends.Simulation.SimulatorTypes

import Data.Map

-- | Local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.SimulatorBuild: " ++ m

-- | Build a 'Simulator' from a 'Circuit'.
--   The simulation engine to use is selected based on the available backing
--   implementations for the 'Circuit' and a user provided preference
buildSimulatorWith :: Maybe Backend -> Circuit -> Simulator
buildSimulatorWith pref c = case (c.backingImplementation, pref) of
  -- Verilator engine
  (BackendFiles xs, Just Verilog)
    | Verilog `elem` fmap fst xs -> buildSimulatorWithVerilator c
  (Netlist nl, Just Verilog) -> buildSimulatorWithVerilator c
  -- Haskell engine
  (Netlist nl, _) -> buildSimulatorWithHaskell simMap c
  -- fall-through case: error
  _ -> err $ "No supported simulation strategy"
  -- recursive build of children simulators
  where simMap = fromList [ (c'.name, buildSimulatorWith pref c')
                          | c' <- getUniqueChildrenCircuits c ]


-- | Build a 'Simulator' from a 'Circuit'.
--   The prefered simulation engine defaults to Haskell
buildSimulator :: Circuit -> Simulator
buildSimulator = buildSimulatorWith Nothing

