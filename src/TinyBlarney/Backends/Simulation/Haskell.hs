{- |

Module      : TinyBlarney.Backends.Simulation.Haskell
Description : TinyBlarney's haskell simulation
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Haskell (
  simulateWithHaskell
) where

import TinyBlarney.Core

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Simulation.Haskell: " ++ m

-- | Simulate a 'Circuit' using in haskell interpretation of the 'Circuit''s
--   netlist
simulateWithHaskell :: Circuit -> IO ()
simulateWithHaskell = err $ "Not implemented yet"
