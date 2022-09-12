{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TinyBlarney.Backends.Verilog (
  writeVerilogModule
) where

import TinyBlarney.Core

import Data.Array
import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as Seq
import Control.Monad.Identity
import Control.Monad.Writer hiding (Product)
import Control.Monad.Reader
import Numeric (showHex)
import Text.PrettyPrint hiding ((<>))
import qualified Text.PrettyPrint as PP ((<>))

-- | local error helper function
err :: String -> a
err m = error $ "TinyBlarney.Backends.Verilog: " ++ m

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
  where
  header = text "module" <+> text nm
                         <+> parens (pCircuitInterface interface) PP.<> semi
  declarations = sep (catMaybes $ map decl netDocs)
  instances = sep (catMaybes $ map inst netDocs)
  alwaysBlock = if null alwsDocs then empty else
        text "always" <+> char '@' PP.<> parens (text "posedge clock")
                      <+> text "begin"
    $+$ wrapRstBlock (sep alwsDocs)
    $+$ text "end"
    where alwsDocs = catMaybes $ alws <$> netDocs
          rstDocs = catMaybes $ rst <$> netDocs
          wrapRstBlock blk = if null rstDocs then blk else
                text "if (reset) begin" $+$ sep rstDocs
            $+$ text "end else begin" $+$ blk $+$ text "end"
  footer = text "endmodule"
  netDocs = toList $ genAllNetDocs netlist

pCircuitInterface :: CircuitInterface -> Doc
pCircuitInterface (PortIn nm w) =
  text "input wire" <+> brackets (int (w-1) PP.<> text ":0") <+> text nm
pCircuitInterface (PortOut nm w) =
  text "output wire" <+> brackets (int (w-1) PP.<> text ":0") <+> text nm
pCircuitInterface (Product xs) = argStyle (pCircuitInterface <$> xs)
pCircuitInterface _ = empty

-- NetDocs helper type
data NetDocs = NetDocs { decl :: Maybe Doc
                       , inst :: Maybe Doc
                       , alws :: Maybe Doc
                       , rst  :: Maybe Doc } deriving Show

newtype GenNetDocs a = GenNetDocs {
  unGenNetDocs :: ReaderT NetlistArray (WriterT (Seq.Seq NetDocs) Identity) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader NetlistArray
           , MonadWriter (Seq.Seq NetDocs) )

genAllNetDocs :: Netlist -> Seq.Seq NetDocs
genAllNetDocs (Netlist nl) =
  runIdentity $ execWriterT (runReaderT (unGenNetDocs gen) nl)
  where gen = mapM genNetDocs (elems nl)

addNetDocs :: NetDocs -> GenNetDocs ()
addNetDocs = tell . Seq.singleton

genNetDocs :: Net -> GenNetDocs ()
genNetDocs n = addNetDocs case n.primitive of
  Constant k w -> dfltNDs { decl = Just $ text "// TODO: constant declaration"}
  And w -> dfltNDs { decl = Just $ text "// TODO: And declaration"}
  Or w -> dfltNDs { decl = Just $ text "// TODO: Or declaration"}
  Interface ifc -> dfltNDs
  p -> err $ "Primitive " ++ show p ++ "not supported"
  where
  dfltNDs = NetDocs { decl = Nothing
                    , inst = Nothing
                    , alws = Nothing
                    , rst  = Nothing }
