module TinyBlarney.Core.NetHelpers (
  remapNetPortInstanceId
, remapNetInstanceId
) where

import TinyBlarney.Core.NetPrimitives

-- | Helper function remap the 'InstanceId's of a 'NetPort'
remapNetPortInstanceId :: (InstanceId -> InstanceId) -> NetPort -> NetPort
remapNetPortInstanceId f (NetPort instId cPath) = NetPort (f instId) cPath
remapNetPortInstanceId f (NetPortInlined prim ins) =
  NetPortInlined prim (remapNetPortInstanceId f <$> ins)

-- | Helper function remap the 'InstanceId's of a 'Net'
remapNetInstanceId :: (InstanceId -> InstanceId) -> Net -> Net
remapNetInstanceId remap net@MkNet{ instanceId = x, inputPorts = y} =
  net { instanceId = remap x
      , inputPorts = remapNetPortInstanceId remap <$> y }
