{-# LANGUAGE OverloadedRecordDot #-}
{-|

Module      : TinyBlarney.Backends.Simulation.Types
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Types (
  SimTime
, Sample
, Signal
, SignalMap
, SimulationInputs
, SimulationOutputs
, AllSimulationOutputs (..)
, PureSimulator
, Simulator
) where

import Data.Map
import Data.Word

import TinyBlarney.Core

-- | A type representing a number of timesteps in a simulation
type SimTime = Word64

-- | A type representing the value of a 'Signal' at a point in time
type Sample = (SimTime, Int)

-- | A type representing a signal
type Signal = [Sample]

-- | A dictionary of named 'Signal's
type SignalMap = Map CircuitInterfacePath Signal

-- | The input signals to a simulation
type SimulationInputs = SignalMap -- ^ dictionary of input signals

-- | The output signals from a simulation
type SimulationOutputs = SignalMap -- ^ dictionary of Output signals

-- | The outputs from a simulation
data AllSimulationOutputs = AllSimulationOutputs {
  effects :: [(SimTime, IO ())] -- ^ Stream of effects
, signals :: SimulationOutputs  -- ^ dictionary of output signals
}

-- | A type defining the interface to a pure simulator
type PureSimulator = SimulationInputs -> AllSimulationOutputs

-- | A type defining the interface to a simulator
type Simulator = SimulationInputs -> IO SimulationOutputs

class Simulatable a where
  simulate :: a -> SimulationInputs -> IO SimulationOutputs

instance Simulatable Simulator where
  simulate s ins = s ins

instance Simulatable PureSimulator where
  simulate s ins = do let allSimOuts = s ins
                      sequence $ fmap snd allSimOuts.effects
                      return allSimOuts.signals
