{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Core.NetHelpers (
  netOutputPaths
, netOutputPath
, netOutput
, netInput
, remapNetPortInstanceId
, remapNetInstanceId
) where

import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

-- | get the output paths of a net
netOutputPaths :: Net -> [CircuitInterfacePath]
netOutputPaths = primOutputPaths . primitive

-- | get the output path of the first output of a net
netOutputPath :: Net -> CircuitInterfacePath
netOutputPath = head . netOutputPaths

-- | get the first 'NetOutput' of a 'Net'
netOutput :: Net -> NetOutput
netOutput n = (n.instanceId, netOutputPath n)

-- | get the first input 'NetPort' of a 'Net'
netInput :: Net -> NetPort
netInput = head . inputPorts

-- | Helper function remap the 'InstanceId's of a 'NetPort'
remapNetPortInstanceId :: (InstanceId -> InstanceId) -> NetPort -> NetPort
remapNetPortInstanceId f (NetPort (instId, cPath)) = NetPort (f instId, cPath)
remapNetPortInstanceId f (NetPortInlined prim ins) =
  NetPortInlined prim (remapNetPortInstanceId f <$> ins)

-- | Helper function remap the 'InstanceId's of a 'Net'
remapNetInstanceId :: (InstanceId -> InstanceId) -> Net -> Net
remapNetInstanceId remap net@MkNet{ instanceId = x, inputPorts = y} =
  net { instanceId = remap x
      , inputPorts = remapNetPortInstanceId remap <$> y }
