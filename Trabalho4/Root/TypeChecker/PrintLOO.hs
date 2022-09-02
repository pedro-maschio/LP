-- File generated by the BNF Converter (bnfc 2.9.3).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Frontend.

module Root.Typechecker.PrintLOO where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Root.Typechecker.AbsLOO

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Root.Typechecker.AbsLOO.Ident where
  prt _ (Root.Typechecker.AbsLOO.Ident i) = doc $ showString i
instance Print Root.Typechecker.AbsLOO.Program where
  prt i = \case
    Root.Typechecker.AbsLOO.Prog classdeclarations -> prPrec i 0 (concatD [prt 0 classdeclarations])

instance Print Root.Typechecker.AbsLOO.ClassDeclaration where
  prt i = \case
    Root.Typechecker.AbsLOO.ClassD id_ extends memberdeclarations -> prPrec i 0 (concatD [doc (showString "class"), prt 0 id_, prt 0 extends, doc (showString "{"), prt 0 memberdeclarations, doc (showString "}")])

instance Print Root.Typechecker.AbsLOO.Extends where
  prt i = \case
    Root.Typechecker.AbsLOO.ExtId id_ -> prPrec i 0 (concatD [doc (showString "extends"), prt 0 id_])
    Root.Typechecker.AbsLOO.ExtObject -> prPrec i 0 (concatD [])

instance Print Root.Typechecker.AbsLOO.MemberDeclaration where
  prt i = \case
    Root.Typechecker.AbsLOO.Attr decl -> prPrec i 0 (concatD [prt 0 decl, doc (showString ";")])
    Root.Typechecker.AbsLOO.Mth type_ id_ decls stms -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 decls, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])

instance Print Root.Typechecker.AbsLOO.Decl where
  prt i = \case
    Root.Typechecker.AbsLOO.Dec type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])

instance Print [Root.Typechecker.AbsLOO.Stm] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Root.Typechecker.AbsLOO.ClassDeclaration] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Root.Typechecker.AbsLOO.MemberDeclaration] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Root.Typechecker.AbsLOO.Decl] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Root.Typechecker.AbsLOO.Exp] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Root.Typechecker.AbsLOO.Stm where
  prt i = \case
    Root.Typechecker.AbsLOO.SDec decl -> prPrec i 0 (concatD [prt 0 decl, doc (showString ";")])
    Root.Typechecker.AbsLOO.SConstInit decl exp -> prPrec i 0 (concatD [doc (showString "const"), prt 0 decl, doc (showString "="), prt 0 exp, doc (showString ";")])
    Root.Typechecker.AbsLOO.SExp exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString ";")])
    Root.Typechecker.AbsLOO.SAss id_ exp -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 exp, doc (showString ";")])
    Root.Typechecker.AbsLOO.SBlock stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stms, doc (showString "}")])
    Root.Typechecker.AbsLOO.SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm])
    Root.Typechecker.AbsLOO.SReturn exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    Root.Typechecker.AbsLOO.SIf exp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString "then"), prt 0 stm1, doc (showString "else"), prt 0 stm2])

instance Print Root.Typechecker.AbsLOO.Exp where
  prt i = \case
    Root.Typechecker.AbsLOO.EOr exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "||"), prt 2 exp2])
    Root.Typechecker.AbsLOO.EAnd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "&&"), prt 3 exp2])
    Root.Typechecker.AbsLOO.ENot exp -> prPrec i 3 (concatD [doc (showString "!"), prt 3 exp])
    Root.Typechecker.AbsLOO.ECon exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "++"), prt 5 exp2])
    Root.Typechecker.AbsLOO.EAdd exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "+"), prt 5 exp2])
    Root.Typechecker.AbsLOO.ESub exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "-"), prt 5 exp2])
    Root.Typechecker.AbsLOO.EMul exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "*"), prt 6 exp2])
    Root.Typechecker.AbsLOO.EDiv exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "/"), prt 6 exp2])
    Root.Typechecker.AbsLOO.EMthCall id_1 id_2 exps -> prPrec i 6 (concatD [prt 0 id_1, doc (showString "."), prt 0 id_2, doc (showString "("), prt 0 exps, doc (showString ")")])
    Root.Typechecker.AbsLOO.ECast id_ exp -> prPrec i 6 (concatD [doc (showString "<"), prt 0 id_, doc (showString ">"), prt 6 exp])
    Root.Typechecker.AbsLOO.ENew id_ -> prPrec i 7 (concatD [doc (showString "new"), prt 0 id_, doc (showString "()")])
    Root.Typechecker.AbsLOO.EInt n -> prPrec i 8 (concatD [prt 0 n])
    Root.Typechecker.AbsLOO.EVar id_ -> prPrec i 8 (concatD [prt 0 id_])
    Root.Typechecker.AbsLOO.EStr str -> prPrec i 8 (concatD [printString str])
    Root.Typechecker.AbsLOO.ETrue -> prPrec i 8 (concatD [doc (showString "true")])
    Root.Typechecker.AbsLOO.EFalse -> prPrec i 8 (concatD [doc (showString "false")])

instance Print Root.Typechecker.AbsLOO.Type where
  prt i = \case
    Root.Typechecker.AbsLOO.Tbool -> prPrec i 0 (concatD [doc (showString "bool")])
    Root.Typechecker.AbsLOO.Tint -> prPrec i 0 (concatD [doc (showString "int")])
    Root.Typechecker.AbsLOO.Tvoid -> prPrec i 0 (concatD [doc (showString "void")])
    Root.Typechecker.AbsLOO.TStr -> prPrec i 0 (concatD [doc (showString "String")])
    Root.Typechecker.AbsLOO.TClass id_ -> prPrec i 0 (concatD [prt 0 id_])
