{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE UndecidableInstances #-}

module TinyBlarney.Core.BuildCircuit (
  buildCircuitInterface
, buildCircuitWith
, buildCircuit
) where

import TinyBlarney.Core.BV
import TinyBlarney.Core.Bit
import TinyBlarney.Core.Bits
import TinyBlarney.Core.FlattenBV
import TinyBlarney.Core.NetHelpers
import TinyBlarney.Core.NetPrimitives
import TinyBlarney.Core.CircuitInterface

import GHC.TypeLits
import Text.PrettyPrint hiding ((<>))

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.BuildCircuit: " ++ m

--------------------------------------------------------------------------------

-- | Build a 'Circuit' from a name, a 'CircuitInterface' and a TinyBlarney
--   circuit function in 'GenCircuit'
buildCircuitWith :: GenCircuit a => String -> CircuitInterface -> a -> Circuit
buildCircuitWith nm ifc f = Circuit { name = nm
                                    , interface = externalNetlistInterface nl
                                    , mNetlist = Just nl
                                    , implementations = mempty }
  where nl = flattenFromRoots $ getCircuitRoots f ifc

-- | Build a 'Circuit' from a name and a TinyBlarney circuit function in
--   'BuildCircuitIfc' and 'GenCircuit'
buildCircuit :: (BuildCircuitIfc a, GenCircuit a) => String -> a -> Circuit
buildCircuit nm f = buildCircuitWith nm (buildCircuitInterface f) f

--------------------------------------------------------------------------------
-- | Build an interface for a circuit function based on its type signature
buildCircuitInterface :: BuildCircuitIfc t => t -> CircuitInterface
buildCircuitInterface = buildCircuitIfc 0

-- backing recursive class implementation

class BuildCircuitIfc t where
  buildCircuitIfc :: Int -> t -> CircuitInterface

instance {-# OVERLAPPABLE #-} Bits a => BuildCircuitIfc a where
  buildCircuitIfc _ _ = externalInterface @a (Just "retVal")

instance (Bits a, BuildCircuitIfc t) => BuildCircuitIfc (a -> t) where
  buildCircuitIfc argN f =
    internalInterface @a nm <> buildCircuitIfc (argN + 1) (f undefined)
    where nm = Just $ "arg" ++ show argN

--------------------------------------------------------------------------------
-- apply circuit function to get root BVs for a blarney circuit

getCircuitRoots :: GenCircuit a => a -> CircuitInterface -> [BV]
getCircuitRoots f ifc = res.roots
  where res = genCircuit dfltAcc inPathAndBVs outPaths f
        inBVs = mkInterfaceBV (flipCircuitInterface ifc) res.outs
        inPathAndBVs = zip (getPortInPaths ifc) inBVs
        outPaths = getPortOutPaths ifc
        dfltAcc = GenCircuitAcc { outs = [], roots = [] }

-- backing recursive class implementation

data GenCircuitAcc = GenCircuitAcc { outs  :: [PathAndBV]
                                   , roots :: [BV] } deriving Show

addOut :: GenCircuitAcc -> CircuitInterfacePath -> BV -> GenCircuitAcc
addOut acc path bv = acc { outs = (path, bv) : acc.outs
                         , roots = bv : acc.roots }

addRoot :: GenCircuitAcc -> BV -> GenCircuitAcc
addRoot acc bv = acc { roots = bv : acc.roots }

class GenCircuit t where
  genCircuit :: GenCircuitAcc -> [PathAndBV] -> [CircuitInterfacePath] -> t
             -> GenCircuitAcc

instance {-# OVERLAPPABLE #-} (Bits a) => GenCircuit a where
  genCircuit acc [] outs x =
    foldl (\a (p, bv) -> addOut a p bv) acc (zip outs (getBVs x))
  --genCircuit acc [] outs x = addOut acc zip outs (getBVs x)
  genCircuit acc ins outs x = err $
    show acc ++ show ins ++ show outs -- ++ show x

instance (Bits a, GenCircuit t) => GenCircuit (a -> t) where
  genCircuit acc ((_, bv):rest) outPaths f =
    genCircuit acc rest outPaths (f . unpack $ AsBit bv)
