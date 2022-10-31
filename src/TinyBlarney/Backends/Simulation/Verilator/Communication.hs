{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|

Module      : TinyBlarney.Backends.Simulation.Verilator.Communication
Stability   : experimental

-}

module TinyBlarney.Backends.Simulation.Verilator.Communication (
  SimReq (..)
, SimCommand
, pattern Evaluate
, pattern Finish
, sendSimReq
, sendSimReqs
, SimRsp (..)
, SimResult
, pattern Evaluated
, pattern Finished
, pattern Unknown
, receiveSimRsps
) where

import GHC.TypeLits
import Foreign hiding (Bits)
import System.IO
import Control.Monad

import TinyBlarney.Misc.Misc
import TinyBlarney.Core hiding (sizeOf)
import TinyBlarney.Backends.Simulation.Types

---- | 'Storable' instance for members of 'Bits'
--instance {-# OVERLAPS #-} Bits a => Storable a where
--  sizeOf _ = ceilDiv (sizeOf (undefined :: a)) 8
--  alignment _ = 0
--  peek ptr = do
--    let nBits = sizeOf (undefined :: a)
--    let nBytes = ceilDiv nBits 8
--    bytes :: [Word8] <- peekArray nBytes (castPtr ptr)
--    return . unpack
--           . (flip unsafeBitNFromInteger) nBits
--           . word8ListToInteger $ bytes
--  poke ptr x = do
--    let bytes :: [Word8] = integerToWord8List . unsafeBitNToInteger . pack $ x
--    pokeArray (castPtr ptr) bytes

--------------------------------------------------------------------------------

-- | The type of requests sent to a TinyBlarney verilator simulation process
data SimReq a = SimReq { simCmd  :: SimCommand -- ^ command to be performed
                       , simTime :: SimTime    -- ^ simulaton timestamp
                       , payload :: a          -- ^ payload for the command
                       }

-- | Type of commands sent to a TinyBlarney verilator simulation
type SimCommand = Word8
-- | Command to terminate the verilator simulation process
pattern Finish :: SimCommand
pattern Finish = 0x00
-- | Command to evaluate the verilator simulation with a given payload
pattern Evaluate :: SimCommand
pattern Evaluate = 0x01

-- | 'Storable' instance for 'SimReq a'
instance Storable a => Storable (SimReq a) where
  sizeOf x = sizeOf x.simCmd + sizeOf x.simTime + sizeOf x.payload
  alignment _ = 0
  peek ptr = do simCmd :: SimCommand <- peek (castPtr ptr)
                let ptrTime = ptr `plusPtr` sizeOf simCmd
                simTime :: SimTime <- peek ptrTime
                let ptrPayload = ptrTime `plusPtr` sizeOf simTime
                payload :: a <- peek ptrPayload
                return SimReq{ simCmd = simCmd
                             , simTime = simTime
                             , payload = payload }
  poke ptr SimReq{..} = do poke (castPtr ptr) simCmd
                           let ptrTime = ptr `plusPtr` sizeOf simCmd
                           poke ptrTime simTime
                           let ptrPayload = ptrTime `plusPtr` sizeOf simTime
                           poke ptrPayload payload

-- | Send a single 'SimReq a' into the provided 'Handle' to the input to a
--   TinyBlarney verilator simulation process
sendSimReq :: Storable a => Handle -> SimReq a -> IO ()
sendSimReq h req = with req \ptr -> hPutBuf h ptr (sizeOf req)

-- | Send a sequence of 'SimReq a's into the provided 'Handle' to the input to a
--   TinyBlarney verilator simulation process
sendSimReqs :: Storable a => Handle -> [SimReq a] -> IO ()
sendSimReqs h reqs = do
  -- allocate raw memory for foreign communication
  -- (prefer malloc+free to alloca for multiple reuses between commands)
  ptr <- malloc
  -- prepare and push out each command
  forM_ reqs \req -> do
    poke ptr req
    hPutBuf h ptr (sizeOf req)
  -- free allocate memory once all commands are handled
  free ptr

--------------------------------------------------------------------------------

-- | The type of responses received from a TinyBlarney verilator simulation
data SimRsp a = SimRsp { simRes  :: SimResult -- ^ returned result type
                       , simTime :: SimTime   -- ^ simulaton timestamp
                       , payload :: a         -- ^ payload for the result
                       }

-- | Type of results received from a TinyBlarney verilator simulation
type SimResult = Word8
-- | Result indicating the simulation terminated
pattern Finished :: SimResult
pattern Finished = 0x00
-- | Result indicating the evaluated output signals for the last simulation step
pattern Evaluated :: SimResult
pattern Evaluated = 0x01
-- | Result indicating an unknown command
pattern Unknown :: SimResult
pattern Unknown = 0x02

-- | 'Storable' instance for 'SimRsp a'
instance Storable a => Storable (SimRsp a) where
  sizeOf x = sizeOf x.simRes + sizeOf x.simTime + sizeOf x.payload
  alignment _ = 0
  peek ptr = do simRes :: SimResult <- peek (castPtr ptr)
                let ptrTime = ptr `plusPtr` sizeOf simRes
                simTime :: SimTime <- peek ptrTime
                let ptrPayload = ptrTime `plusPtr` sizeOf simTime
                payload :: a <- peek ptrPayload
                return SimRsp{ simRes = simRes
                             , simTime = simTime
                             , payload = payload }
  poke ptr SimRsp{..} = do poke (castPtr ptr) simRes
                           let ptrTime = ptr `plusPtr` sizeOf simRes
                           poke ptrTime simTime
                           let ptrPayload = ptrTime `plusPtr` sizeOf simTime
                           poke ptrPayload payload

-- | Receive a sequence of 'SimRsp a's from the provided 'Handle' to the output
--   from a TinyBlarney verilator simulation process
receiveSimRsps :: Storable a => Handle -> IO [SimRsp a]
receiveSimRsps h = do
  -- allocate raw memory for foreign communication
  -- (prefer malloc+free to alloca for multiple reuses between commands)
  ptr :: Ptr (SimRsp a) <- malloc
  -- receive all responses
  rsps <- unfoldWhileM (\x -> x.simRes /= Finished)
                       do hGetBufSome h ptr (sizeOf (undefined :: SimRsp a))
                          peek ptr
  -- free allocate memory once all commands are handled
  free ptr
  -- return the list of responses
  return rsps
