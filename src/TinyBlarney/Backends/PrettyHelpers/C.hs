{- |

Module      : TinyBlarney.Backends.PrettyHelpers.C
Description : Helper to pretty print C and C++
Stability   : experimental

-}

module TinyBlarney.Backends.PrettyHelpers.C (
  cFunCall
, cAssign
, cNew
, cPostIncr
, cDirectAccess
, cIndirectAccess
, cFor
, cWhile
, cDoWhile
, cSwitch
, cDef
, cFunDef
, cDelete
, cReturn
) where

import Prelude hiding ((<>))
import Text.PrettyPrint
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

type CType = String
type CIdent = String
type CTypedIdent = (CType, CIdent)
type CExpr = Doc
type CStmt = Doc

-- C expressions
--------------------------------------------------------------------------------

cFunCall :: CExpr -> [CExpr] -> CExpr
cFunCall f args = hsep [f, parens (commaSep $ args)]

cAssign :: CExpr -> CExpr -> CExpr
cAssign lhs rhs = hsep [lhs, equals, rhs]

cNew :: CType -> CExpr
cNew t = text "new" <+> text t

cPostIncr :: CIdent -> Int -> CExpr
cPostIncr nm 1 = text nm <> text "++"
cPostIncr nm (-1) = text nm <> text "--"
cPostIncr nm n | n < 0 = text nm <+> text "-=" <+> int n
               | otherwise = text nm <+> text "+=" <+> int n

cDirectAccess :: CIdent -> CIdent -> CExpr
cDirectAccess obj field = text obj <> char '.' <> text field

cIndirectAccess :: CIdent -> CIdent -> CExpr
cIndirectAccess obj field = text obj <> text "->" <> text field

-- C statements
--------------------------------------------------------------------------------

cFor :: CExpr -> CExpr -> CExpr -> [CStmt] -> CStmt
cFor initE testE updtE stmts =
  text "for" <+> parens (semiSep [initE, testE, updtE]) <+> braces (vcat stmts)

cWhile :: CExpr -> [CStmt] -> CStmt
cWhile testE stmts = text "while" <+> parens testE <+> braces (vcat stmts)

cDoWhile :: CExpr -> [CStmt] -> CStmt
cDoWhile testE stmts =
  text "do" <+> braces (vcat stmts) <+> text "while" <+> parens testE <+> semi

cSwitch :: CExpr -> [(CExpr, [CStmt])] -> Maybe [CStmt] -> CStmt
cSwitch testE cases dflt = text "switch" <+> parens testE <+> braces (vcat alts)
  where alt (matchE, stmts) = vcat [ text "case" <+> matchE <> colon
                                   , nest 2 $ vcat stmts ]
        dfltCase (Just stmts) = vcat [ text "default" <> colon
                                     , nest 2 $ vcat stmts ]
        dfltCase _ = empty
        alts = (alt <$> cases) ++ [dfltCase dflt]

cDef :: CTypedIdent -> Maybe CExpr -> CStmt
cDef (cType, cIdent) (Just initExpr) =
  cAssign (text cType <+> text cIdent) initExpr <> semi
cDef (cType, cIdent) Nothing = text cType <+> text cIdent <> semi

cFunDef :: CType -> CIdent -> [CTypedIdent] -> [CStmt] -> CStmt
cFunDef fType fIdent fArgs fStmts =
  text fType <+> text fIdent <+> parens (commaSep args) <+> braces body
  where args = (\(t, nm) -> text t <+> text nm) <$> fArgs
        body = vcat fStmts

cDelete :: CIdent -> CStmt
cDelete nm = text "delete" <+> text nm <> semi

cReturn :: CExpr -> CStmt
cReturn x = text "return" <+> x <> semi
