{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Tests.SimpleMerge.Test (test) where

import Distribution.TestSuite
import TinyBlarney

import Data.Map
import GHC.Generics
import Control.Monad

orMerge3 :: (Bit 1, Bit n) -> (Bit 1, Bit n) -> (Bit 1, Bit n) -> Bit n
orMerge3 x y z = bitMerge MStratOr [x, y, z]

test :: TestInstance
test = TestInstance {
    run = do putStrLn "building circuit"
             putStrLn "--------------------------------------------------"
             putStrLn $ show c
             putStrLn "--------------------------------------------------"
             forM_ vs \v -> putStrLn v
             putStrLn "--------------------------------------------------"
             sim <- buildSimulatorWith (Just Verilog) c
             putStrLn "simulator built"
             return $ Finished Pass
  , name = "test merge"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right test
  }
  where c = buildCircuit "orMerge3" (orMerge3 @8)
        vs = generateVerilog c
