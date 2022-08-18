module TinyBlarney.Backends (
  writeVerilog
) where

import TinyBlarney.Core

writeVerilog :: String -> Circuit -> String
writeVerilog nm c = nm ++ "\n" ++ show c ++ "\n- no verilog yet"
