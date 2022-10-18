{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Core.NetHelpers (
  netInputPaths
, netInputs
, netOutputsInfo
, netOutputWidths
, netOutputPaths
, netOutputs
, netOutput
, netPorts
, remapNetConnectionInstanceId
, remapNetInputConnectionInstanceId
, remapNetInstanceId
, netNamesWith
, netNames
, externalNetlistInterface
) where

import TinyBlarney.Core.BasicTypes

import Data.Map hiding (elems)
import Data.List
import Data.Array
import Text.PrettyPrint

-- * Individual 'Net' helpers
--------------------------------------------------------------------------------

-- | get the input paths of a net
netInputPaths :: Net -> [CircuitInterfacePath]
netInputPaths = primInputPaths . primitive

-- | get all 'NetInput' input ports of a 'Net'
netInputs :: Net -> [NetInput]
netInputs n = zip (repeat n.instanceId) (fst <$> n.inputConnections)

-- | get the output info of a net
netOutputsInfo :: Net -> [(CircuitInterfacePath, BitWidth)]
netOutputsInfo = primOutputsInfo . primitive

-- | get the output widths of a net
netOutputWidths :: Net -> [BitWidth]
netOutputWidths = primOutputWidths . primitive

-- | get the output paths of a net
netOutputPaths :: Net -> [CircuitInterfacePath]
netOutputPaths = primOutputPaths . primitive

-- | get all 'NetOutput's of a 'Net'
netOutputs :: Net -> [NetOutput]
netOutputs n = zip (repeat n.instanceId) (netOutputPaths n)

-- | get the first 'NetOutput' of a 'Net'
netOutput :: Net -> NetOutput
netOutput = head . netOutputs

-- | get the 'NetPort's of a 'Net'
netPorts :: Net -> [NetPort]
netPorts n = netInputs n ++ netOutputs n

-- | Helper function remap the 'InstanceId's of a 'NetConnection'
remapNetConnectionInstanceId :: (InstanceId -> InstanceId) -> NetConnection
                             -> NetConnection
remapNetConnectionInstanceId f (NetConnection (instId, cPath)) =
  NetConnection (f instId, cPath)
remapNetConnectionInstanceId f (NetConnectionInlined prim ins) =
  NetConnectionInlined prim (remapNetConnectionInstanceId f <$> ins)

-- | Helper function remap the 'InstanceId's of a 'NetInput'
remapNetInputConnectionInstanceId :: (InstanceId -> InstanceId)
                                  -> NetInputConnection
                                  -> NetInputConnection
remapNetInputConnectionInstanceId f (p, np) =
  (p, remapNetConnectionInstanceId f np)

-- | Helper function remap the 'InstanceId's of a 'Net'
remapNetInstanceId :: (InstanceId -> InstanceId) -> Net -> Net
remapNetInstanceId f net@Net{ instanceId = x, inputConnections = y} =
  net { instanceId = f x
      , inputConnections = remapNetInputConnectionInstanceId f <$> y }

-- * 'Netlist' helpers
--------------------------------------------------------------------------------

-- | Derive a map of Net names given a Netlist and a function to process name
--   hints
netNamesWith :: ([String] -> Maybe String) -> Netlist
             -> Map NetPort String
netNamesWith deriveName nl =
  fromList $ concatMap f (elems nl)
  where f :: Net -> [(NetPort, String)]
        f n = let ret x = (x, (render $ prettyNetPort x) ++ suffix)
                  suffix = case deriveName [] of Just s -> "_" ++ s
                                                 _ -> ""
                  outs = [ ret nOut | nOut <- netOutputs n ]
                  ins = [ ret nIns | nIns <- netInputs n
                                   , case n.primitive of Interface _ -> True
                                                         _ -> False ]
              in ins ++ outs

-- | Derive a map of Net names given a Netlist
netNames :: Netlist -> Map NetOutput String
netNames = netNamesWith dfltDerive
  where dfltDerive :: [String] -> Maybe String
        dfltDerive [] = Nothing
        dfltDerive xs = Just (intercalate "_" $ reverse xs)

-- | Return the exposed external 'CircuitInterface' of the given 'Circuit'
externalNetlistInterface :: Netlist -> CircuitInterface
externalNetlistInterface nl = mconcat (snd <$> ifcs)
  where ifcs = sortOn fst [ (nId, metaInstanceId nId $ flipCircuitInterface ifc)
                          | n@Net{ instanceId = nId
                                 , primitive = Interface ifc } <- elems nl ]
