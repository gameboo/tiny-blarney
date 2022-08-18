{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TinyBlarney.Core.NetPrimitives (
  InstanceId
, Net (..)
, NetPort (..)
, Primitive (..)
, PrimName
, CustomNetlist
, PrimitiveInfo (..)
, ifcUnaryOp
, ifcBinaryOp
, primitiveInfo
) where

import TinyBlarney.Core.CircuitInterface

type InstanceId = Int

data NetPort = NetPort InstanceId CircuitInterfacePath
             | NetPortInlined Primitive [NetPort]
             deriving Show

data Net = MkNet { instanceId :: InstanceId
                 , primitive :: Primitive
                 , inputPorts :: [NetPort] }
         deriving Show

type PrimName = String

-- | tiny-blarney's language primitives
data Primitive =
    -- | sized constant value
    Constant Integer BitWidth
    -- | logical and of 2 inputs
  | And BitWidth
    -- | logical or of 2 inputs
  | Or BitWidth
    -- | custom primitive with possible custom netlist
  | Custom { name :: PrimName
           , interface :: CircuitInterface
           , mNetlist :: Maybe CustomNetlist }
    -- | a circuit interface primitive
    --   BVs with this primitive are flatten roots or flatten leaves based on
    --   the polarity of the port described
    --   The embedded 'CircuitInterface''s polarity can be flipped to obtain the
    --   circuit's interface as perceived from the outside of the circuit
    --   (example:
    --     A ciruit with an Interface primitive with a PortIn "A" and a PortOut
    --     "B" produces values at A and consumes values at B.
    --     The environment using this circuit will produce into it through B and
    --     consume out of it from A.
    --   )
  | Interface CircuitInterface
  deriving Show

newtype CustomNetlist = MkCustomNetlist () -- TODO
instance Show CustomNetlist where show _ = "CustomNetlist"

-- | general information on a primitive
data PrimitiveInfo = MkPrimitiveInfo {
  -- | the circuit interface of the primitive (its inputs and outputs...)
  interface :: CircuitInterface
}

-- | 'CircuitInterface' for 1-input 1-output circuits a.k.a. unary op.
ifcUnaryOp :: BitWidth -> CircuitInterface
ifcUnaryOp w = PortIn "in" w <> PortOut "out" w

-- | 'CircuitInterface' for 2-inputs 1-output circuits a.k.a. binary op.
ifcBinaryOp :: BitWidth -> CircuitInterface
ifcBinaryOp w = PortIn "in0" w <> PortIn "in1" w <> PortOut "out" w

-- | document 'PrimitiveInfo' for any 'Primitive'
primitiveInfo :: Primitive -> PrimitiveInfo
primitiveInfo (Constant _ w) = MkPrimitiveInfo { interface = PortOut "out" w }
primitiveInfo (And w) = MkPrimitiveInfo { interface = ifcBinaryOp w }
primitiveInfo (Or w) = MkPrimitiveInfo { interface = ifcBinaryOp w }
primitiveInfo p@Custom{} = MkPrimitiveInfo { interface = p.interface }
primitiveInfo (Interface ifc) = MkPrimitiveInfo { interface = ifc }
