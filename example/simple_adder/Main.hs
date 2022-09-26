{-# LANGUAGE DataKinds #-}

import TinyBlarney

halfAdder :: Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
halfAdder x y = (sum, carry)
  where sum = x `bitXor` y
        carry = x `bitAnd` y

fullAdder :: Bit 1 -> Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
fullAdder cIn x y = (sum, cOut)
  where (s0, c0) = halfAdder x y
        (sum, c1) = halfAdder s0 cIn
        cOut = c0 `bitOr` c1

carryChainAdder :: Bit 1 -> Bit n -> Bit n -> (Bit n, Bit 1)
carryChainAdder cIn x y = (unsafeFromBitList ss, last cs)
  where ins = zip (unsafeToBitList x) (unsafeToBitList y)
        allFas = scanl (\(_, c) (x, y) -> fullAdder c x y) (bitZero, cIn) ins
        (ss, cs) = unzip $ tail allFas

main:: IO ()
main = do
  putStrLn $ show c
  putStrLn "--------------------------------------------------"
  putStrLn v
  where c = buildCircuit "carryChainAdder" $ carryChainAdder @8
        v = writeVerilogModule c
