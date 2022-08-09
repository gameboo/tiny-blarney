import TinyBlarney

mkAndABOrC :: (BV :: *) -> (BV :: *) -> (BV :: *) -> (BV :: *)
mkAndABOrC a b c = mkOrBV (mkAndBV a b) c

main:: IO ()
main = putStrLn $ writeVerilog "myMkAndABOrC" (buildCircuit mkAndABOrC)
