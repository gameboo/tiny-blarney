{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module TinyBlarney.Core.FlattenBV (
  flattenFromRoots
) where

import TinyBlarney.Core.BV
import TinyBlarney.Core.NetPrimitives

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Functor.Identity
import qualified Data.IntSet as IntSet

--------------------------------------------------------------------------------
-- flattening of the untyped bitvector representation to the netlist graph
-- representation

type FlattenBV = StateT FlattenS (WriterT FlattenW Identity)

type FlattenS = IntSet.IntSet

type FlattenW = [Net]

execFlattenBV :: FlattenBV a -> FlattenS -> (FlattenS, FlattenW, a)
execFlattenBV m s0 = (s, w, x)
  where f = runIdentity . runWriterT . (flip runStateT) s0
        ((x, s), w) = f m

getVisited :: FlattenBV FlattenS
getVisited = get

putVisited :: FlattenS -> FlattenBV ()
putVisited = put

addNet :: Net -> FlattenBV ()
addNet = lift . tell . (:[])

flattenBV :: BV -> FlattenBV NetPort
flattenBV MkBV{ primitive = p@(Constant _ _) } = return $ NetPortInlined p []
flattenBV bv = do
  visited <- getVisited
  when (not $ bv.instanceId `IntSet.member` visited) do
    putVisited $ IntSet.insert bv.instanceId visited
    inPorts <- mapM flattenBV (snd <$> bv.receivedSignals)
    addNet MkNet { instanceId = bv.instanceId
                 , primitive  = bv.primitive
                 , inputPorts = inPorts }
  return $ NetPort bv.instanceId bv.exposedPath

flattenBV_ :: BV -> FlattenBV ()
flattenBV_ bv = flattenBV bv >> return ()

flattenFromRoots :: [BV] -> [Net]
flattenFromRoots rootBVs = nl
  where (_, nl, _) = execFlattenBV (mapM flattenBV_ rootBVs) mempty
