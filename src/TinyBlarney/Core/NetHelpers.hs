{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Core.NetHelpers (
  netInputPaths
, netInputsAsNetOutput
, netInputs
, netOutputPaths
, netOutputs
, netOutput
, remapNetPortInstanceId
, remapNetInputInstanceId
, remapNetInstanceId
, netNames
) where

import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

import Data.Map hiding (elems)
import Data.Array
import Text.PrettyPrint

-- * Individual 'Net' helpers
--------------------------------------------------------------------------------

-- | get the input paths of a net
netInputPaths :: Net -> [CircuitInterfacePath]
netInputPaths = primInputPaths . primitive

-- | get all input 'NetOutput's of a 'Net'
netInputsAsNetOutput :: Net -> [NetOutput]
netInputsAsNetOutput n = zip (repeat n.instanceId) (netInputPaths n)

-- | get all input 'NetInput's of a 'Net'
netInputs :: Net -> [NetInput]
netInputs = inputPorts

-- | get the output paths of a net
netOutputPaths :: Net -> [CircuitInterfacePath]
netOutputPaths = primOutputPaths . primitive

-- | get all 'NetOutput's of a 'Net'
netOutputs :: Net -> [NetOutput]
netOutputs n = zip (repeat n.instanceId) (netOutputPaths n)

-- | get the first 'NetOutput' of a 'Net'
netOutput :: Net -> NetOutput
netOutput = head . netOutputs

-- | Helper function remap the 'InstanceId's of a 'NetPort'
remapNetPortInstanceId :: (InstanceId -> InstanceId) -> NetPort -> NetPort
remapNetPortInstanceId f (NetPort (instId, cPath)) = NetPort (f instId, cPath)
remapNetPortInstanceId f (NetPortInlined prim ins) =
  NetPortInlined prim (remapNetPortInstanceId f <$> ins)

-- | Helper function remap the 'InstanceId's of a 'NetInput'
remapNetInputInstanceId :: (InstanceId -> InstanceId) -> NetInput -> NetInput
remapNetInputInstanceId f (p, np) = (p, remapNetPortInstanceId f np)

-- | Helper function remap the 'InstanceId's of a 'Net'
remapNetInstanceId :: (InstanceId -> InstanceId) -> Net -> Net
remapNetInstanceId remap net@MkNet{ instanceId = x, inputPorts = y} =
  net { instanceId = remap x
      , inputPorts = remapNetInputInstanceId remap <$> y }

-- * 'Netlist' helpers
--------------------------------------------------------------------------------

netNames :: (String -> [String] -> String) -> Netlist -> Map NetOutput String
netNames deriveName Netlist{netlistArray = nl} =
  fromList $ concatMap f (elems nl)
  where f :: Net -> [(NetOutput, String)]
        f n = let ret x = (x, deriveName (render $ prettyNetOutput x) [])
                  outs = [ ret nOut | nOut <- netOutputs n ]
                  ins = [ ret nOut | nOut <- netInputsAsNetOutput n
                                   , case n.primitive of Interface _ -> True
                                                         _ -> False ]
              in ins ++ outs
