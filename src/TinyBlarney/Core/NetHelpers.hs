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
, netlistNamesWith
, netlistNames
, interfaceNamesWith
, interfaceNames
, defaultNameDerive
, externalNetlistInterface
) where

import TinyBlarney.Core.BasicTypes

import Data.Map hiding (elems)
import Data.List
import Data.Array
import Data.Maybe
import Text.PrettyPrint

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.NetHelpers: " ++ m

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

-- | Derive a map of names given a 'Netlist' and a function to process name
--   hints.
netlistNamesWith :: ([String] -> Maybe String) -> Netlist -> Map NetPort String
netlistNamesWith deriveName nl = fromList $ concat allNames
  where allNames = ifcNames : [ f n | n <- elems nl
                              , case n.primitive of Interface _ -> False
                                                    _ -> True ]
        f n = [ (x, (render $ prettyNetPort x) ++ sfx n) | x <- netOutputs n ]
        sfx _ = case deriveName [] of Just s -> "_" ++ s
                                      _ -> ""
        (ifcId, ifc) = externalNetlistInterface nl
        -- could have flipped the ifc but it wouldn't change anything for names
        ifcPfx = Just "ifc"
        ifcNamesRaw = toList $ interfaceNamesWith deriveName ifcPfx ifc
        ifcNames = fmap (\(path, nm) -> ((ifcId, path), nm)) ifcNamesRaw

-- | Derive a map of names given a 'Netlist'.
netlistNames :: Netlist -> Map NetPort String
netlistNames = netlistNamesWith defaultNameDerive

-- | Derive a map of names given a 'CircuitInterface' and a function to process
--   name hints.
interfaceNamesWith :: ([String] -> Maybe String) -> Maybe String
                   -> CircuitInterface
                   -> Map CircuitInterfacePath String
interfaceNamesWith deriveName mPfx ifc = fromList names
  where names = onCircuitInterfaceLeaves f ifc
        f ctxt = ( ctxt.path
                 , intercalate "_" $ catMaybes [ mPfx
                                               , Just $ show ctxt.path
                                               , ctxt.implicitTag
                                               , deriveName ctxt.nameHints ] )

-- | Derive a map of names given a 'CircuitInterface'.
interfaceNames :: Maybe String -> CircuitInterface
               -> Map CircuitInterfacePath String
interfaceNames = interfaceNamesWith defaultNameDerive

-- | Default derivation of names from a collection of name hints
defaultNameDerive :: [String] -> Maybe String
defaultNameDerive [] = Nothing
defaultNameDerive xs = Just (intercalate "_" $ reverse xs)

-- | Return the exposed external 'CircuitInterface' of the given 'Circuit'
--externalNetlistInterface :: Netlist -> CircuitInterface
--externalNetlistInterface nl = mconcat (snd <$> ifcs)
--  where ifcs = sortOn fst [ (nId, metaInstanceId nId $ flipCircuitInterface ifc)
--                          | n@Net{ instanceId = nId
--                                 , primitive = Interface ifc } <- elems nl ]
externalNetlistInterface :: Netlist -> (InstanceId, CircuitInterface)
externalNetlistInterface nl = if length ifcs == 1 then head ifcs
                                                  else fail
  where ifcs = sortOn fst [ (nId, metaInstanceId nId $ flipCircuitInterface ifc)
                          | n@Net{ instanceId = nId
                                 , primitive = Interface ifc } <- elems nl ]
        fail = err $ "There should be exactly one interface per Netlist\n"
                     ++ show nl
