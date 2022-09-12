{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Core.NetHelpers (
  getOutputPaths
, getOutputPath
, getNetOutput
, getNetInput
, remapNetPortInstanceId
, remapNetInstanceId
) where

import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

-- | get the output paths of a net
getOutputPaths :: Net -> [CircuitInterfacePath]
getOutputPaths = outputPaths . primitiveInfo . primitive

-- | get the output path of the first output of a net
getOutputPath :: Net -> CircuitInterfacePath
getOutputPath = head . getOutputPaths

-- | get the first NetOutput of the Net
getNetOutput :: Net -> NetOutput
getNetOutput n = (n.instanceId, getOutputPath n)

-- | get the first input NetPort of the Net
getNetInput :: Net -> NetPort
getNetInput = head . inputPorts

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
