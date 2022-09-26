{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE UndecidableInstances #-}

module TinyBlarney.Core.BuildCircuit (
  Circuit (..)
, prettyCircuit
, buildCircuitInterface
, externalCircuitInterface
, buildCircuitWith
, buildCircuit
) where

import TinyBlarney.Core.BV
import TinyBlarney.Core.Bit
import TinyBlarney.Core.Bits
import TinyBlarney.Core.FlattenBV
import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

import Data.Array
import GHC.TypeLits
import Text.PrettyPrint hiding ((<>))

--------------------------------------------------------------------------------
-- | Build an interface for a circuit function based on its type signature
class BuildCircuitIfc t where
  buildCircuitIfc :: Int -> t -> CircuitInterface

instance {-# OVERLAPPABLE #-} Bits a => BuildCircuitIfc a where
  buildCircuitIfc _ _ = externalInterface @a (Just "retVal")

instance (Bits a, BuildCircuitIfc t) => BuildCircuitIfc (a -> t) where
  buildCircuitIfc argN f =
    internalInterface @a nm <> buildCircuitIfc (argN + 1) (f undefined)
    where nm = Just $ "arg" ++ show argN

buildCircuitInterface :: BuildCircuitIfc t => t -> CircuitInterface
buildCircuitInterface = buildCircuitIfc 0

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

instance {-# OVERLAPPABLE #-} (Bits a) => GenCircuit a where
  genCircuit x [] path acc = addOut path acc (pack x).bv

instance (Bits a, GenCircuit t) => GenCircuit (a -> t) where
  -- XXX should traverse an ifc and update path as we go
  genCircuit f (bv : bvs) path acc = genCircuit (f . unpack $ AsBit bv) bvs path acc

getCircuitRoots :: GenCircuit a => a -> CircuitInterface -> [BV]
getCircuitRoots f ifc = rootBVs
  where (outBVs, rootBVs) = genCircuit f inBVs NoStep ([], [])
        inBVs = mkInterfaceBV ifc outBVs

--------------------------------------------------------------------------------
-- produce Circuit Netlist and Interface

data Circuit = Circuit { name :: String
                       , netlist :: Netlist }

externalCircuitInterface :: Circuit -> CircuitInterface
externalCircuitInterface Circuit{..} =
 head [ flipCircuitInterface ifc
      | MkNet{primitive = Interface ifc} <- elems netlist.netlistArray ]

prettyCircuit :: Circuit -> Doc
prettyCircuit circuit =
  hang (text "Circuit -" <+> text circuit.name) 2 (pIfc $+$ pNl)
  where ifc = externalCircuitInterface circuit
        pIfc = text "- interface:" <+> nest 2 (prettyCircuitInterface ifc)
        pNl  = text "- netlist:" <+> nest 2 (prettyNetlist circuit.netlist)

instance Show Circuit where
  show = render . prettyCircuit

buildCircuitWith :: GenCircuit a => String -> CircuitInterface -> a -> Circuit
buildCircuitWith nm ifc f = Circuit { name = nm, netlist = nl }
  where nl = flattenFromRoots $ getCircuitRoots f (flipCircuitInterface ifc)

buildCircuit :: (BuildCircuitIfc a, GenCircuit a) => String -> a -> Circuit
buildCircuit nm f = buildCircuitWith nm (buildCircuitInterface f) f
