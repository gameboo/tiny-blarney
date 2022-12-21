{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Tests.SimpleDelay.Test (test) where

import Distribution.TestSuite
import TinyBlarney

import Data.Map
import GHC.Generics
import Control.Monad

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
  , name = "test delay"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right test
  }
  where c = buildCircuit "delay" (bitDelay @8)
        vs = generateVerilog c
