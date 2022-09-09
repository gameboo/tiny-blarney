{-# LANGUAGE RecordWildCards #-}

module TinyBlarney.Backends.Verilog (
  writeVerilogModule
) where

import TinyBlarney.Core

import Data.Maybe
import Control.Monad.Identity
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Numeric (showHex)
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

-- exported API
--------------------------------------------------------------------------------
writeVerilogModule :: String -> Circuit -> String
writeVerilogModule nm c = render $ prettyVerilogModule nm c

-- Internal helpers
--------------------------------------------------------------------------------

-- pretty helpers
dot = char '.'
spaces n = hcat $ replicate n space
hexInt n = text (showHex n "")
argStyle = sep . punctuate comma

-- pretty module
prettyVerilogModule :: String -> Circuit -> Doc
prettyVerilogModule nm Circuit{..} =     header
                                     $+$ declarations
                                     $+$ instances
                                     $+$ alwaysBlock
                                     $+$ footer
  where header =     text "module" <+> text nm
                 <+> parens (pCircuitInterface interface) PP.<> semi
        footer = text "endmodule"
        declarations = sep (catMaybes $ map decl netVs)
        instances = sep (catMaybes $ map inst netVs)
        alwaysBlock =
              text "always" <+> char '@' PP.<> parens (text "posedge clock")
                            <+> text "begin"
          $+$ text "if (reset) begin" $+$ sep (catMaybes $ map rst netVs)
          $+$ text "end else begin" $+$ sep (catMaybes $ map alws netVs)
          $+$ text "end"
          $+$ text "end"
        netVs = toNetVerilogs netlist

pCircuitInterface :: CircuitInterface -> Doc
pCircuitInterface (PortIn nm w) =
  text "input wire" <+> brackets (int (w-1) PP.<> text ":0") <+> text nm
pCircuitInterface (PortOut nm w) =
  text "output wire" <+> brackets (int (w-1) PP.<> text ":0") <+> text nm
pCircuitInterface (Product xs) = argStyle (pCircuitInterface <$> xs)
pCircuitInterface _ = empty

-- NetVerilog helper type
data NetVerilog = NetVerilog { decl :: Maybe Doc
                             , inst :: Maybe Doc
                             , alws :: Maybe Doc
                             , rst  :: Maybe Doc }

type NetVeriloger = ReaderT Netlist (WriterT [NetVerilog] Identity)
toNetVerilogs :: Netlist -> [NetVerilog]
toNetVerilogs ns = runIdentity $ execWriterT $ runReaderT netVeriloger ns
  where netVeriloger :: NetVeriloger ()
        netVeriloger = do return ()
