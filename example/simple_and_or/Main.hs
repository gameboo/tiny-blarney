{-# LANGUAGE DataKinds     #-}

import TinyBlarney

mkAndABOrC :: Bit n -> Bit n -> Bit n -> Bit n
mkAndABOrC a b c = (a .&. b) .|. c

main:: IO ()
main = putStrLn $ writeVerilog "myMkAndABOrC" (buildCircuit $ mkAndABOrC @8)
