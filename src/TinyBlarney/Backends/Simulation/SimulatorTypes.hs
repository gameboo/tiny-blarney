{-|

Module      : TinyBlarney.Backends.Simulation.SimulatorTypes
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.SimulatorTypes (
  SimTime
, Sample
, Signal
, SignalMap
, SimulatorIfc (..)
, Simulator
) where

import Data.Map

-- | A type representing a number of timesteps in a simulation
type SimTime = Int

-- | A type representing the value of a 'Signal' at a point in time
type Sample = (SimTime, Int)

-- | A type representing a signal
type Signal = [Sample]

-- | A dictionary of named 'Signal's
type SignalMap = Map String Signal

-- | A 'Simulator''s interface
data SimulatorIfc = SimulatorIfc {
  --effects :: [(SimTime, IO ())] -- ^ Stream of effects
  outputs :: SignalMap          -- ^ dictionary of output signals
}

-- | A type defining the interface to a simulator
type Simulator = SignalMap -> SimulatorIfc
