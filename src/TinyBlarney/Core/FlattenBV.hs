{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module TinyBlarney.Core.FlattenBV (
  flattenFromRoots
) where

import TinyBlarney.Core.BV
import TinyBlarney.Core.NetHelpers
import TinyBlarney.Core.BasicTypes

import Data.Map qualified as Map
import Data.IntSet qualified as IntSet
import Data.Sequence qualified as Seq
import Data.Foldable
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer hiding (Product)
import Control.Monad.Except

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Core.FlattenBV: " ++ m

--------------------------------------------------------------------------------
-- flattening of the untyped bitvector representation to the netlist graph
-- representation

data FlattenS = FlattenS {
    visited :: IntSet.IntSet
  , interfaceId :: Maybe InstanceId
  , implicitInputs :: Map.Map String (BitWidth, [NetInput])
} deriving Show

type FlattenW = Seq.Seq Net

newtype FlattenBV a = FlattenBV {
  runFlattenBV :: StateT FlattenS (WriterT FlattenW (Either String)) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadError String
           , MonadWriter FlattenW
           , MonadState FlattenS )

execFlattenBV :: FlattenBV a -> FlattenS -> (FlattenS, FlattenW, a)
execFlattenBV m s0 = case f $ runFlattenBV m of
  Left e -> err e
  Right ((x, s), w) -> (s, w, x)
  where f = runWriterT . (flip runStateT) s0

isMarkedAsVisited :: InstanceId -> FlattenBV Bool
isMarkedAsVisited i = do s <- get
                         return $ i `IntSet.member` s.visited

markAsVisited :: InstanceId -> FlattenBV ()
markAsVisited i = modify' \s -> s { visited = i `IntSet.insert` s.visited }

setInterfaceInstanceId :: InstanceId -> FlattenBV ()
setInterfaceInstanceId i = do
  s <- get
  case s.interfaceId of
    Nothing -> put s { interfaceId = Just i }
    Just _ -> throwError $ "Only one Interface allowed per flattened netlist.\n"
                           ++ show s

addImplicitInputs :: BV -> FlattenBV ()
addImplicitInputs bv = do
  s <- get
  -- for each implicit input (as a list comprehension)
  sequence_ [ do
    -- handle size mismatch
    case Map.lookup tag s.implicitInputs of
      Just (w', _) | w /= w' -> throwError msg
      _ -> return ()
    -- accumulate the new implicit input
    put s { implicitInputs = Map.insertWith (\(_, [x]) (w, os) -> (w, x:os))
                                            tag (w, [(bv.instanceId, path)])
                                            s.implicitInputs }
    | (path, Port In w, tag) <- getImplicitPortIns (primInterface bv.primitive)
    ]
  where msg = "Implicit signals with the same tag should have the same width"

addNet :: Net -> FlattenBV ()
addNet = tell . Seq.singleton

flattenBV :: BV -> FlattenBV NetConnection
--flattenBV BV{ primitive = p@(Constant _ _) } =
--  return $ NetConnectionInlined p []
flattenBV bv = do
  -- check if the current BV has been visited before
  wasVisited <- isMarkedAsVisited bv.instanceId
  -- if not ...
  when (not wasVisited) do
    -- first, mark the BV as visited
    markAsVisited bv.instanceId
    -- then, based on the primitive ...
    case bv.primitive of
      -- check if this BV is the Interface to the netlist
      Interface _ -> setInterfaceInstanceId bv.instanceId
      -- otherwise, gather implicit interface signals if any
      -- XXX TODO output signals as well
      _ -> addImplicitInputs bv
    -- gather the incoming connections by recursively flattening the BV's inputs
    inConns <- sequence [ do nConn <- flattenBV b
                             return (path, nConn)
                        | (path, b) <- bv.receivedSignals ]
    -- add a corresponding Net to the accumulated netlist
    addNet Net { instanceId = bv.instanceId
               , primitive  = bv.primitive
               , inputConnections = inConns }
  -- return the output connection represented by the current BV
  return $ NetConnection (bv.instanceId, bv.exposedPath)

flattenBV_ :: BV -> FlattenBV ()
flattenBV_ bv = flattenBV bv >> return ()

flattenFromRoots :: [BV] -> Netlist
flattenFromRoots rootBVs = runSTArray do
  mnl <- newListArray (0, length nl - 1)
                      [remapNetInstanceId (mapping !) n | n <- nl]
  -- XXX here work on mutable netlist XXX
  let ifcId = case fS.interfaceId of
                Just i -> mapping ! i
                _ -> err "No interface was found upon flattenBV"
  -- add implicit inputs to the interface
  ifcNet <- readArray mnl ifcId
  let ifcImpInPorts = [ metaImplicit tag (Port Out w)
                      | (tag, (w, _)) <- Map.toList fS.implicitInputs ]
  let ifc' = primInterface ifcNet.primitive <> Product ifcImpInPorts
  let ifcNet' = Net { instanceId = ifcNet.instanceId
                    , primitive = Interface ifc'
                    , inputConnections = ifcNet.inputConnections }
  writeArray mnl ifcId ifcNet'
  -- prepare new interface implicit input connections
  sequence_ [
      do net <- readArray mnl nId
         let conn = NetConnection (ifcId, ifcPath)
         let net' = net{inputConnections = (nPath, conn):net.inputConnections}
         writeArray mnl nId net'
    | (ifcPath, _, tag) <- getImplicitPortOuts ifc'
    , (nId, nPath) <- case Map.lookup tag fS.implicitInputs of
        Just (_, netIns) -> (\(x, y) -> (mapping ! x, y)) <$> netIns
        _ -> err "should not happen" ]

  -- return final netlist
  return mnl
  ------------------------
  where
    -- flatten BVs into a list of Nets
    dfltS = FlattenS { visited = mempty
                     , interfaceId = Nothing
                     , implicitInputs = mempty }
    (fS, fW, _) = execFlattenBV (mapM flattenBV_ rootBVs) dfltS
    seqNl = fW
    nl = toList seqNl
    -- for remapping instanceIds to a compact range starting from 0
    minInstId = IntSet.findMin fS.visited
    maxInstId = IntSet.findMax fS.visited
    mapping :: UArray InstanceId InstanceId
    mapping = array (minInstId, maxInstId)
                    [(n.instanceId, x) | (n, x) <- zip nl [0..]]
