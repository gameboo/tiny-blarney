{-# LANGUAGE BlockArguments #-}

{- |

Module      : TinyBlarney.Misc.PrettyHelpers.Verilog
Description : Helper to pretty print Verilog
Stability   : experimental

-}

module TinyBlarney.Misc.PrettyHelpers.Verilog (
  VIdent
, VBitWidth
, VStmt
, VPort
, VPortDir (..)
, VPortWidth
, VWireOrReg (..)
, VInitVal (..)
, vModDef
, vIdentDef
, vIntLit
, vDontCare
, vAssign
, vConcat
, vReplicate
, vBeginEnd
, vFunCall
, vAlwaysBlock
, vModInst
) where

import Prelude hiding ((<>))
import Text.PrettyPrint
import Data.List
import Numeric (showHex)

-- pretty helpers
spaces :: Int -> Doc
spaces n = hcat $ replicate n space
hexInt :: Integer -> Doc
hexInt n = text (showHex n "")
commaSep :: [Doc] -> Doc
commaSep = punctSep comma
semiSep :: [Doc] -> Doc
semiSep = punctSep semi
punctSep :: Doc -> [Doc] -> Doc
punctSep s = sep . punctuate s

type VIdent = String
type VBitWidth = Int
type VStmt = Doc
type VPort = (VPortDir, VPortWidth, VIdent)
data VPortDir = In | Out
type VPortWidth = Int
data VWireOrReg = Wire | Reg
data VInitVal = IntInitVal Integer | DontCareInitVal | NoInitVal

vModDef :: VIdent -> [VPort] -> [VStmt] -> VStmt
vModDef mNm mArgs mStmts = vcat
  [ text "module" <+> text mNm <+> parens (commaSep args) <> semi
  , nest 2 $ vcat mStmts
  , text "endmodule" ]
  where args = mArg <$> mArgs
        mArg (pDir, w, nm) = hsep
          [ text case pDir of In -> "input"
                              Out -> "output"
          , text "wire"
          , if w > 1 then brackets (int (w-1) <> text ":0") else empty
          , text nm ]

-- | Verilog identifier def
vIdentDef :: VIdent -> VWireOrReg -> VBitWidth -> VInitVal -> Doc
vIdentDef nm wireOrReg w initVal =
  wireOrRegDoc <+> widthDoc <+> text nm <+> initDoc <> semi
  where
    wireOrRegDoc = case wireOrReg of Wire -> text "wire"
                                     Reg  -> text "reg"
    widthDoc = if w > 1 then brackets (int (w-1) <> text ":0") else mempty
    initDoc = case initVal of
      NoInitVal -> mempty
      IntInitVal x -> equals <+> vIntLit x w
      DontCareInitVal -> equals <+> vDontCare w

vIntLit :: Integer -> VBitWidth -> Doc
vIntLit v w = int w <> text "'h" <> hexInt v

vDontCare :: VBitWidth -> Doc
vDontCare w = int w <> text "'b" <> text (replicate w 'x')

vConcat :: [Doc] -> Doc
vConcat = braces . commaSep

vReplicate :: Doc -> [Doc] -> Doc
vReplicate n args = braces (n <> vConcat args)

vAssign :: Doc -> Doc -> Doc
vAssign lhs rhs = (text "assign" <+> lhs <+> equals <+> rhs) <> semi

vBeginEnd :: [Doc] -> Doc
vBeginEnd stmts = sep [ text "begin", nest 2 (vcat stmts), text "end" ]

vFunCall :: Doc -> [Doc] -> Doc
vFunCall funNm args = funNm  <+> parens (nest 2 $ commaSep args) <> semi

vAlwaysBlock :: [Doc] -> [Doc] -> Doc
vAlwaysBlock [] _ = empty
vAlwaysBlock sensitivity stmts =
  text "always" <+> char '@' <+> (parens $ commaSep sensitivity)
                <+> vBeginEnd stmts

vModInst :: VIdent -> VIdent -> [VStmt] -> VStmt
vModInst modNm instNm args =
  text modNm <+> text instNm <+> parens (nest 2 $ commaSep args) <> semi
