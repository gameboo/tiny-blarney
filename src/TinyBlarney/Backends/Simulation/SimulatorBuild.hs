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
import TinyBlarney.Backends.Simulation.Types
import TinyBlarney.Backends.Simulation.Haskell
import TinyBlarney.Backends.Simulation.Verilator

import Data.Map

-- | Local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.SimulatorBuild: " ++ m

-- | Build a 'Simulator' from a 'Circuit'.
--   The simulation engine to use is selected based on the available backing
--   implementations for the 'Circuit' and a user provided preference
buildSimulatorWith :: Maybe Backend -> Circuit -> IO Simulator
buildSimulatorWith pref c = case (c.backingImplementation, pref) of
  -- Verilator engine
  (BackendFiles xs, Just Verilog)
    | Verilog `elem` fmap fst xs -> do
    putStrLn $ "Generating a simulator using verilator simulation backend"
    buildSimulatorWithVerilator c
  (Netlist nl, Just Verilog) -> do
    putStrLn $ "Generating a simulator using verilator simulation backend"
    buildSimulatorWithVerilator c
  -- Haskell engine
  (Netlist nl, _) -> do
    putStrLn $ "Generating a simulator using in-haskell simulation backend"
    simMap <- fromList <$> sequence [ do sim <- buildSimulatorWith pref c'
                                         return (c'.name, sim)
                                    | c' <- getUniqueChildrenCircuits c ]
    buildSimulatorWithHaskell simMap c
  -- fall-through case: error
  _ -> err $ "No supported simulation strategy"
  -- recursive build of children simulators
  where simMap = fromList [ (c'.name, buildSimulatorWith pref c')
                          | c' <- getUniqueChildrenCircuits c ]


-- | Build a 'Simulator' from a 'Circuit'.
--   The prefered simulation engine defaults to Haskell
buildSimulator :: Circuit -> IO Simulator
buildSimulator = buildSimulatorWith Nothing
