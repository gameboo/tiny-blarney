{- |

Module      : TinyBlarney.Backends.Simulation.Haskell
Description : TinyBlarney's haskell simulation
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Haskell (
  buildSimulatorWithHaskell
) where

import TinyBlarney.Core
import TinyBlarney.Backends.Simulation.SimulatorTypes

import Data.Map

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Haskell: " ++ m

-- | Build a 'Simulator' using in haskell interpretation of the 'Circuit''s
--   netlist
buildSimulatorWithHaskell :: Map String Simulator -> Circuit -> Simulator
buildSimulatorWithHaskell = err $ "Not implemented yet"
