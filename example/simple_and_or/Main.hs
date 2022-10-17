{-# LANGUAGE DataKinds     #-}

import TinyBlarney

mkAndABOrC :: Bit n -> Bit n -> Bit n -> Bit n
mkAndABOrC a b c = (a `bitAnd` b) `bitOr` c

main:: IO ()
main = do
  putStrLn "generate tiny-blarney circuit for mkAndABOrC @8"
  putStrLn "----"
  putStrLn $ show c
  putStrLn "----"
  putStrLn "generate verilog"
  putStrLn "----"
  putStrLn v
  putStrLn "----"
  putStrLn "build simulator"
  putStrLn "----"
  sim <- buildSimulatorWith (Just Verilog) c
  putStrLn "simulator built"
  where c = buildCircuit "myMkAndABOrC" $ mkAndABOrC @8
        v = generateTopVerilog c
