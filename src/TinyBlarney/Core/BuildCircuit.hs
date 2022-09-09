{-# LANGUAGE RecordWildCards #-}

module TinyBlarney.Core.BuildCircuit (
  Circuit (..)
, prettyCircuit
, buildCircuit
) where

import TinyBlarney.Core.BV
import TinyBlarney.Core.Bit
import TinyBlarney.Core.FlattenBV
import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

import GHC.TypeLits
import Text.PrettyPrint hiding ((<>))

--------------------------------------------------------------------------------
-- apply circuit function to get root BVs for a blarney circuit

class GetCircuitRoots t where
  getCircuitRoots :: t -> [BV] -> [BV]

instance GetCircuitRoots (Bit n) where
  getCircuitRoots (AsBit bv) [] = [bv]

instance GetCircuitRoots t => GetCircuitRoots (Bit n -> t) where
  getCircuitRoots f (bv : bvs) = getCircuitRoots (f $ AsBit bv) bvs

--------------------------------------------------------------------------------
-- generate circuit interface for a blarney circuit

class BuildCircuitIfc t where
  buildCircuitIfc :: Int -> t -> CircuitInterface

instance KnownNat n => BuildCircuitIfc (Bit n) where
  buildCircuitIfc _ _ = PortOut "retVal" $ valueOf @n

instance (KnownNat n, BuildCircuitIfc t) => BuildCircuitIfc (Bit n -> t) where
  buildCircuitIfc argN f =    PortIn ("arg" ++ show argN) (valueOf @n)
                           <> (buildCircuitIfc (argN + 1) $ f undefined)

getCircuitInterface :: BuildCircuitIfc t => t -> CircuitInterface
getCircuitInterface = buildCircuitIfc 0

--------------------------------------------------------------------------------
-- produce Circuit Netlist and Interface

data Circuit = Circuit { interface :: CircuitInterface
                       , netlist   :: Netlist }

prettyCircuit :: Circuit -> Doc
prettyCircuit Circuit{..} = hang (text "Circuit") 2 (ifc $+$ nl)
  where ifc = text "- interface:" <+> nest 2 (prettyCircuitInterface interface)
        nl  = text "- netlist:" <+> nest 2 (prettyNetlist netlist)

instance Show Circuit where
  show = render . prettyCircuit

buildCircuit :: (BuildCircuitIfc a, GetCircuitRoots a) => a -> Circuit
buildCircuit f = Circuit { interface = ifc, netlist = nl }
  where ifc = getCircuitInterface f
        ifcBVs = mkInterfaceBV $ flipCircuitInterface ifc
        nl = flattenFromRoots $ getCircuitRoots f ifcBVs
