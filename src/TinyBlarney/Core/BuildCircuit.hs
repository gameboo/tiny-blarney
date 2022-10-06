{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE UndecidableInstances #-}

module TinyBlarney.Core.BuildCircuit (
  buildCircuitInterface
, buildCircuitWith
, buildCircuit
, customInstanceWithCircuit
, customInstanceWith
, customInstance
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
                                    , backingImplementation = Netlist nl }
  where nl = flattenFromRoots $ getCircuitRoots f ifc

-- | Build a 'Circuit' from a name and a TinyBlarney circuit function in
--   'BuildCircuitIfc' and 'GenCircuit'
buildCircuit :: (BuildCircuitIfc a, GenCircuit a) => String -> a -> Circuit
buildCircuit nm f = buildCircuitWith nm (buildCircuitInterface f) f

customInstanceWithCircuit :: GenCircuit a => Circuit -> a
customInstanceWithCircuit circuit = genWrapper circuit inPaths []
  where inPaths = getPortInPaths circuit.interface

customInstanceWith :: (BuildCircuitIfc a, GenCircuit a) => String -> a -> a
customInstanceWith nm f = customInstanceWithCircuit $ buildCircuit nm f

customInstance :: (BuildCircuitIfc a, GenCircuit a) => String -> a
customInstance nm = f
  where f = customInstanceWithCircuit c
        c = Circuit { name = nm
                    , interface = buildCircuitInterface f
                    , backingImplementation = BackendFiles [] }

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
  where res = genToCircuit dfltAcc inPathAndBVs outPaths f
        inBVs = mkInterfaceBV (flipCircuitInterface ifc) res.outs
        inPathAndBVs = zip (getPortInPaths ifc) inBVs
        outPaths = getPortOutPaths ifc
        dfltAcc = ToCircuitAcc { outs = [], roots = [] }

-- backing recursive class implementation

data ToCircuitAcc = ToCircuitAcc { outs  :: [PathAndBV]
                                 , roots :: [BV] } deriving Show

addOut :: ToCircuitAcc -> CircuitInterfacePath -> BV -> ToCircuitAcc
addOut acc path bv = acc { outs = (path, bv) : acc.outs
                         , roots = bv : acc.roots }

addRoot :: ToCircuitAcc -> BV -> ToCircuitAcc
addRoot acc bv = acc { roots = bv : acc.roots }

class GenCircuit t where
  genToCircuit :: ToCircuitAcc -> [PathAndBV] -> [CircuitInterfacePath] -> t
               -> ToCircuitAcc
  genWrapper :: Circuit -> [CircuitInterfacePath] -> [PathAndBV] -> t

instance {-# OVERLAPPABLE #-} (Bits a) => GenCircuit a where
  genToCircuit acc [] outs x =
    foldl (\a (p, bv) -> addOut a p bv) acc (zip outs (toBVs x))
  genToCircuit acc ins outs x = err $ show acc ++ show ins ++ show outs

  genWrapper circuit [] rcvBVs = fromBVs $ mkCustomBV circuit (reverse rcvBVs)

instance (Bits a, GenCircuit t) => GenCircuit (a -> t) where
  genToCircuit acc ((_, bv):rest) outPaths f =
    genToCircuit acc rest outPaths (f . unpack $ AsBit bv)

  genWrapper circuit ps rcvBVs x =
    genWrapper circuit morePaths (newRcvBVs ++ rcvBVs)
    where xBVs = toBVs x
          n = length xBVs
          (paths, morePaths) = splitAt n ps
          newRcvBVs = zip paths xBVs
