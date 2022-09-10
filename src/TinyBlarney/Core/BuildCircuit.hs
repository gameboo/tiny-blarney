{-# LANGUAGE RecordWildCards #-}

module TinyBlarney.Core.BuildCircuit (
  Circuit (..)
, prettyCircuit
, getCircuitInterface
, buildCircuitWith
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
-- get a circuit interface for a circuit function based on its type signature

class GetCircuitIfc t where
  getCircuitIfc :: Int -> t -> CircuitInterface

instance KnownNat n => GetCircuitIfc (Bit n) where
  getCircuitIfc _ _ = PortOut "retVal" $ valueOf @n

instance (KnownNat n, GetCircuitIfc t) => GetCircuitIfc (Bit n -> t) where
  getCircuitIfc argN f =    PortIn ("arg" ++ show argN) (valueOf @n)
                         <> (getCircuitIfc (argN + 1) $ f undefined)

getCircuitInterface :: GetCircuitIfc t => t -> CircuitInterface
getCircuitInterface = getCircuitIfc 0

--------------------------------------------------------------------------------
-- apply circuit function to get root BVs for a blarney circuit

addOut :: CircuitInterfacePath -> ([PathAndBV], [BV]) -> BV
       -> ([PathAndBV], [BV])
addOut path (outs, roots) bv = ((path, bv):outs, bv:roots)

addRoot :: ([PathAndBV], [BV]) -> BV -> ([PathAndBV], [BV])
addRoot (outs, roots) bv = (outs, bv:roots)

class GenCircuit t where
  genCircuit :: t -> [BV] -> CircuitInterfacePath -> ([PathAndBV], [BV])
             -> ([PathAndBV], [BV])

instance GenCircuit (Bit n) where
  genCircuit (AsBit bv) [] path acc = addOut path acc bv

instance GenCircuit t => GenCircuit (Bit n -> t) where
  -- XXX should traverse an ifc and update path as we go
  genCircuit f (bv : bvs) path acc = genCircuit (f $ AsBit bv) bvs path acc

getCircuitRoots :: GenCircuit a => a -> CircuitInterface -> [BV]
getCircuitRoots f ifc = rootBVs
  where (outBVs, rootBVs) = genCircuit f inBVs NoStep ([], [])
        inBVs = mkInterfaceBV ifc outBVs

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

buildCircuitWith :: GenCircuit a => CircuitInterface -> a -> Circuit
buildCircuitWith ifc f = Circuit { interface = ifc, netlist = nl }
  where nl = flattenFromRoots $ getCircuitRoots f (flipCircuitInterface ifc)

buildCircuit :: (GetCircuitIfc a, GenCircuit a) => a -> Circuit
buildCircuit f = buildCircuitWith (getCircuitInterface f) f
