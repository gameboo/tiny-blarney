{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TinyBlarney.Tests.SimpleAndOr.Test (test) where

import Distribution.TestSuite
import TinyBlarney

mkAndABOrC :: Bit n -> Bit n -> Bit n -> Bit n
mkAndABOrC a b c = (a `bitAnd` b) `bitOr` c

dfltTest :: TestInstance
dfltTest = TestInstance {
    run = return $ Finished Pass
  , name = "default test"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right dfltTest
  }

test :: TestInstance
test = dfltTest {
    run = do putStrLn "building circuit"
             putStrLn (show c)
             putStrLn "generating verilog"
             putStrLn v
             putStrLn "building simulator"
             sim <- buildSimulatorWith (Just Verilog) c
             putStrLn "simulator built"
             return $ Finished Pass
  , name = "test mkAndABOrC"
  , tags = []
  , options = []
  , setOption = \_ _ -> Right test
  }
  where c = buildCircuit "myMkAndABOrC" (mkAndABOrC @8)
        v = generateTopVerilog c
