{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedRecordDot #-}

import TinyBlarney

import Data.Map
import GHC.Generics
import Control.Monad

halfAdder :: Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
halfAdder x y = (sum, carry)
  where sum = x `bitXor` y
        carry = x `bitAnd` y

instHalfAdder :: Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
--instHalfAdder = halfAdder
--instHalfAdder = customInstanceWithCircuit $ buildCircuit "halfAdder" halfAdder
instHalfAdder = customInstanceWith "halfAdder" halfAdder
--instHalfAdder = customInstance "halfAdder"

fullAdder :: Bit 1 -> Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
fullAdder cIn x y = (sum, cOut)
--fullAdder cIn x y = (sum, iterate bitInvert cOut !! 10000)
  where (s0, c0) = instHalfAdder x y
        (sum, c1) = instHalfAdder s0 cIn
        cOut = c0 `bitOr` c1

instFullAdder :: Bit 1 -> Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
--instFullAdder = fullAdder
--instFullAdder = customInstanceWithCircuit $ buildCircuit "fullAdder" fullAdder
instFullAdder = customInstanceWith "fullAdder" fullAdder
--instFullAdder = customInstance "fullAdder"

rawCarryChainAdder :: Bit 1 -> Bit n -> Bit n -> (Bit n, Bit 1)
rawCarryChainAdder cIn x y = (unsafeFromBitList ss, last cs)
  where ins = zip (unsafeToBitList x) (unsafeToBitList y)
        allFas =
          scanl (\(_, c) (x, y) -> instFullAdder c x y) (bitZero, cIn) ins
        (ss, cs) = unzip $ tail allFas

data AdderRes n = AdderRes { sum :: Bit n
                           , carry :: Bit 1 } deriving (Generic, Bits)

carryChainAdder :: Bit 1 -> Bit n -> Bit n -> AdderRes n
carryChainAdder cIn x y =
  (\(sum, carry) -> AdderRes sum carry) $ rawCarryChainAdder cIn x y

main:: IO ()
main = do
  putStrLn $ show c
  putStrLn "--------------------------------------------------"
  forM_ vs \v -> putStrLn v
  putStrLn "--------------------------------------------------"
  sim <- buildSimulatorWith (Just Verilog) c
  putStrLn "simulator built"
  putStrLn "--------------------------------------------------"
  let cInSig = zip [0..10] (repeat 0)
  let xSig = zip [0..10] [0..10]
  let ySig = zip [0..10] [0..10] -- [y * 10 | y <- [0..10]]
  let simIns = fromList (zip (getPortInPaths c.interface) [cInSig, xSig, ySig])
  --let simIns = mempty
  putStrLn $ "Stimulus: " ++ show simIns
  simOuts <- simulate sim simIns
  putStrLn $ show simOuts
  where c = buildCircuit "carryChainAdder" $ carryChainAdder @8
        vs = generateVerilog c
