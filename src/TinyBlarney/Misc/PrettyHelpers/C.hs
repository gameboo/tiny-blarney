{- |

Module      : TinyBlarney.Misc.PrettyHelpers.C
Description : Helper to pretty print C and C++
Stability   : experimental

-}

module TinyBlarney.Misc.PrettyHelpers.C (
  CType
, CIdent
, CTypedIdent
, CExpr
, CStmt
, cFunCall
, cAssign
, cNew
, cPostIncr
, cOrAccum
, cDirectAccess
, cIndirectAccess
, cFor
, cWhile
, cDoWhile
, cSwitch
, cIf
, cCast
, cDeref
, cNot
, cOrs
, cAnds
, cDef
, cFunDef
, cDelete
, cReturn
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

type CType = String
type CIdent = String
type CTypedIdent = (CType, CIdent)
type CExpr = Doc
type CStmt = Doc

-- C expressions
--------------------------------------------------------------------------------

cBlock :: [CStmt] -> CStmt
cBlock stmts = hang lbrace 2 (vcat stmts) $$ rbrace

cFunCall :: CExpr -> [CExpr] -> CExpr
cFunCall f args = f <+> parens (commaSep $ args)

cAssign :: CExpr -> CExpr -> CExpr
cAssign lhs rhs = hang (lhs <+> equals) 2 rhs

cNew :: CType -> CExpr
cNew t = text "new" <+> text t

cPostIncr :: CIdent -> Int -> CExpr
cPostIncr nm 1 = text nm <> text "++"
cPostIncr nm (-1) = text nm <> text "--"
cPostIncr nm n | n < 0 = text nm <+> text "-=" <+> int n
               | otherwise = text nm <+> text "+=" <+> int n

cOrAccum :: CIdent -> CExpr -> CExpr
cOrAccum lhs rhs = hang (text lhs <+> text "|=") 2 rhs

cDirectAccess :: CIdent -> CIdent -> CExpr
cDirectAccess obj field = text obj <> char '.' <> text field

cIndirectAccess :: CIdent -> CIdent -> CExpr
cIndirectAccess obj field = text obj <> text "->" <> text field

-- C statements
--------------------------------------------------------------------------------

cFor :: CExpr -> CExpr -> CExpr -> [CStmt] -> CStmt
cFor initE testE updtE stmts =
  text "for" <+> parens (semiSep [initE, testE, updtE]) $$ cBlock stmts

cWhile :: CExpr -> [CStmt] -> CStmt
cWhile testE stmts = text "while" <+> parens testE $$ cBlock stmts

cDoWhile :: CExpr -> [CStmt] -> CStmt
cDoWhile testE stmts = text "do" $$ cBlock stmts
                                 $$ text "while" <+> parens testE <+> semi

cSwitch :: CExpr -> [(CExpr, [CStmt])] -> Maybe [CStmt] -> CStmt
cSwitch testE cases dflt = text "switch" <+> parens testE $$ cBlock alts
  where alt (matchE, stmts) = vcat [ text "case" <+> matchE <> colon
                                   , nest 2 $ vcat stmts ]
        dfltCase (Just stmts) = vcat [ text "default" <> colon
                                     , nest 2 $ vcat stmts ]
        dfltCase _ = empty
        alts = (alt <$> cases) ++ [dfltCase dflt]

cIf :: [(CExpr, [CStmt])] -> Maybe [CStmt] -> CStmt
cIf alts fallThrough
  | length alts > 0 = cIf1 alts fallThrough
  | otherwise = cElse NoWrapElse fallThrough

cIf1 :: [(CExpr, [CStmt])] -> Maybe [CStmt] -> CStmt
cIf1 ((testE, stmts):[]) fThrough = text "if" <+> parens testE
                                              $$ cBlock stmts
                                              $$ cElse WrapElse fThrough
cIf1 ((testE, stmts):alts) fThrough = text "if" <+> parens testE
                                                $$ cBlock stmts
                                                $$ text "else"
                                                <+> cIf1 alts fThrough

data WrapElse = WrapElse | NoWrapElse
cElse :: WrapElse -> Maybe [CStmt] -> CStmt
cElse WrapElse (Just stmts) = text "else" $$ cBlock stmts
cElse NoWrapElse (Just stmts) = vcat stmts
cElse _ _ = empty

cCast :: CType -> CExpr -> CExpr
cCast t e = parens (text t) <> parens e

cDeref :: CExpr -> CExpr
cDeref e = char '*' <> parens e

cNot :: CExpr -> CExpr
cNot e = char '!' <> parens e

cOrs :: [CExpr] -> CExpr
cOrs = sep . intersperse (text "||") . (parens <$>)

cAnds :: [CExpr] -> CExpr
cAnds = sep . intersperse (text "&&") . (parens <$>)

cDef :: CTypedIdent -> Maybe CExpr -> CStmt
cDef (cType, cIdent) (Just initExpr) =
  cAssign (text cType <+> text cIdent) initExpr <> semi
cDef (cType, cIdent) Nothing = text cType <+> text cIdent <> semi

cFunDef :: CType -> CIdent -> [CTypedIdent] -> [CStmt] -> CStmt
cFunDef fType fIdent fArgs fStmts =
  text fType <+> text fIdent <+> parens (commaSep args) $$ cBlock fStmts
  where args = (\(t, nm) -> text t <+> text nm) <$> fArgs

cDelete :: CIdent -> CStmt
cDelete nm = text "delete" <+> text nm <> semi

cReturn :: CExpr -> CStmt
cReturn x = text "return" <+> x <> semi
