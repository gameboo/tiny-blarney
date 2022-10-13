{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module TinyBlarney.Core.FlattenBV (
  flattenFromRoots
) where

import TinyBlarney.Core.BV
import TinyBlarney.Core.NetHelpers
import TinyBlarney.Core.BasicTypes

import Data.Foldable
import qualified Data.Sequence as Seq
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.IntSet as IntSet

--------------------------------------------------------------------------------
-- flattening of the untyped bitvector representation to the netlist graph
-- representation

type FlattenS = IntSet.IntSet

type FlattenW = Seq.Seq Net

newtype FlattenBV a = FlattenBV {
  unFlattenBV :: StateT FlattenS (WriterT FlattenW Identity) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter FlattenW
           , MonadState FlattenS )

execFlattenBV :: FlattenBV a -> FlattenS -> (FlattenS, FlattenW, a)
execFlattenBV m s0 = (s, w, x)
  where f = runIdentity . runWriterT . (flip runStateT) s0
        ((x, s), w) = f $ unFlattenBV m

getVisited :: FlattenBV FlattenS
getVisited = get

putVisited :: FlattenS -> FlattenBV ()
putVisited = put

addNet :: Net -> FlattenBV ()
addNet = tell . Seq.singleton

flattenBV :: BV -> FlattenBV NetPort
flattenBV BV{ primitive = p@(Constant _ _) } = return $ NetPortInlined p []
flattenBV bv = do
  visited <- getVisited
  when (not $ bv.instanceId `IntSet.member` visited) do
    putVisited $ IntSet.insert bv.instanceId visited
    inPorts <- sequence [ do nPort <- flattenBV b
                             return (path, nPort)
                        | (path, b) <- bv.receivedSignals ]
    addNet Net { instanceId = bv.instanceId
               , primitive  = bv.primitive
               , inputPorts = inPorts }
  return $ NetPort (bv.instanceId, bv.exposedPath)

flattenBV_ :: BV -> FlattenBV ()
flattenBV_ bv = flattenBV bv >> return ()

flattenFromRoots :: [BV] -> Netlist
flattenFromRoots rootBVs = runSTArray do
  mnl <- newListArray (0, length nl - 1)
                      [remapNetInstanceId (mapping !) n | n <- nl]
  -- XXX here work on mutable netlist XXX
  -- return final netlist
  return mnl
  ------------------------
  where
    -- flatten BVs into a list of Nets
    (visited, seq_nl, _) = execFlattenBV (mapM flattenBV_ rootBVs) mempty
    nl = toList seq_nl
    -- for remapping instanceIds to a compact range starting from 0
    minInstId = IntSet.findMin visited
    maxInstId = IntSet.findMax visited
    mapping :: UArray InstanceId InstanceId
    mapping = array (minInstId, maxInstId)
                    [(n.instanceId, x) | (n, x) <- zip nl [0..]]
