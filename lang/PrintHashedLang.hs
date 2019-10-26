{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintHashedLang.
--   Generated by the BNF converter.

module PrintHashedLang where

import qualified AbsHashedLang
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

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
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsHashedLang.KWDataPattern where
  prt _ (AbsHashedLang.KWDataPattern i) = doc (showString i)

instance Print AbsHashedLang.PIdent where
  prt _ (AbsHashedLang.PIdent (_,i)) = doc (showString i)

instance Print AbsHashedLang.Problem where
  prt i e = case e of
    AbsHashedLang.Problem blocks -> prPrec i 0 (concatD [prt 0 blocks])

instance Print AbsHashedLang.Block where
  prt i e = case e of
    AbsHashedLang.BlockVariable variabledeclss -> prPrec i 0 (concatD [doc (showString "variables"), doc (showString ":"), doc (showString "{"), prt 0 variabledeclss, doc (showString "}")])
    AbsHashedLang.BlockConstant constantdeclss -> prPrec i 0 (concatD [doc (showString "constants"), doc (showString ":"), doc (showString "{"), prt 0 constantdeclss, doc (showString "}")])
    AbsHashedLang.BlockConstraint constraintdeclss -> prPrec i 0 (concatD [doc (showString "constraints"), doc (showString ":"), doc (showString "{"), prt 0 constraintdeclss, doc (showString "}")])
    AbsHashedLang.BlockLet letdeclss -> prPrec i 0 (concatD [doc (showString "let"), doc (showString ":"), doc (showString "{"), prt 0 letdeclss, doc (showString "}")])
    AbsHashedLang.BlockMinimize exp -> prPrec i 0 (concatD [doc (showString "minimize"), doc (showString ":"), doc (showString "{"), prt 0 exp, doc (showString "}")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsHashedLang.Block] where
  prt = prtList

instance Print AbsHashedLang.Z where
  prt i e = case e of
    AbsHashedLang.Z n -> prPrec i 0 (concatD [prt 0 n])

instance Print AbsHashedLang.R where
  prt i e = case e of
    AbsHashedLang.R d -> prPrec i 0 (concatD [prt 0 d])

instance Print AbsHashedLang.Number where
  prt i e = case e of
    AbsHashedLang.NumInt z -> prPrec i 0 (concatD [prt 0 z])
    AbsHashedLang.NumDouble r -> prPrec i 0 (concatD [prt 0 r])

instance Print AbsHashedLang.Val where
  prt i e = case e of
    AbsHashedLang.ValFile str -> prPrec i 0 (concatD [doc (showString "File"), doc (showString "("), prt 0 str, doc (showString ")")])
    AbsHashedLang.ValDataset str1 str2 -> prPrec i 0 (concatD [doc (showString "Dataset"), doc (showString "("), prt 0 str1, doc (showString ","), prt 0 str2, doc (showString ")")])
    AbsHashedLang.ValPattern kwdatapattern -> prPrec i 0 (concatD [doc (showString "Pattern"), doc (showString "("), prt 0 kwdatapattern, doc (showString ")")])
    AbsHashedLang.ValRandom -> prPrec i 0 (concatD [doc (showString "Random")])
    AbsHashedLang.ValLiteral number -> prPrec i 0 (concatD [prt 0 number])

instance Print AbsHashedLang.Dim where
  prt i e = case e of
    AbsHashedLang.Dim n -> prPrec i 0 (concatD [doc (showString "["), prt 0 n, doc (showString "]")])

instance Print AbsHashedLang.Shape where
  prt i e = case e of
    AbsHashedLang.ShapeScalar -> prPrec i 0 (concatD [])
    AbsHashedLang.Shape1D dim -> prPrec i 0 (concatD [prt 0 dim])
    AbsHashedLang.Shape2D dim1 dim2 -> prPrec i 0 (concatD [prt 0 dim1, prt 0 dim2])
    AbsHashedLang.Shape3D dim1 dim2 dim3 -> prPrec i 0 (concatD [prt 0 dim1, prt 0 dim2, prt 0 dim3])

instance Print AbsHashedLang.VariableDecl where
  prt i e = case e of
    AbsHashedLang.VariableNoInit pident shape -> prPrec i 0 (concatD [prt 0 pident, prt 0 shape])
    AbsHashedLang.VariableWithInit pident shape val -> prPrec i 0 (concatD [prt 0 pident, prt 0 shape, doc (showString "="), prt 0 val])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsHashedLang.VariableDecl] where
  prt = prtList
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [[AbsHashedLang.VariableDecl]] where
  prt = prtList

instance Print AbsHashedLang.ConstantDecl where
  prt i e = case e of
    AbsHashedLang.ConstantDecl pident shape val -> prPrec i 0 (concatD [prt 0 pident, prt 0 shape, doc (showString "="), prt 0 val])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsHashedLang.ConstantDecl] where
  prt = prtList
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [[AbsHashedLang.ConstantDecl]] where
  prt = prtList

instance Print AbsHashedLang.LetDecl where
  prt i e = case e of
    AbsHashedLang.LetDecl pident exp -> prPrec i 0 (concatD [prt 0 pident, doc (showString "="), prt 0 exp])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsHashedLang.LetDecl] where
  prt = prtList
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [[AbsHashedLang.LetDecl]] where
  prt = prtList

instance Print AbsHashedLang.Bound where
  prt i e = case e of
    AbsHashedLang.ConstantBound pident -> prPrec i 0 (concatD [prt 0 pident])
    AbsHashedLang.NumberBound number -> prPrec i 0 (concatD [prt 0 number])

instance Print AbsHashedLang.ConstraintDecl where
  prt i e = case e of
    AbsHashedLang.ConstraintLower exp bound -> prPrec i 0 (concatD [prt 0 exp, doc (showString ">="), prt 0 bound])
    AbsHashedLang.ConstraintUpper exp bound -> prPrec i 0 (concatD [prt 0 exp, doc (showString "<="), prt 0 bound])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsHashedLang.ConstraintDecl] where
  prt = prtList
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [[AbsHashedLang.ConstraintDecl]] where
  prt = prtList

instance Print AbsHashedLang.RotateAmount where
  prt i e = case e of
    AbsHashedLang.RA1D n -> prPrec i 0 (concatD [prt 0 n])
    AbsHashedLang.RA2D n1 n2 -> prPrec i 0 (concatD [doc (showString "("), prt 0 n1, doc (showString ","), prt 0 n2, doc (showString ")")])
    AbsHashedLang.RA3D n1 n2 n3 -> prPrec i 0 (concatD [doc (showString "("), prt 0 n1, doc (showString ","), prt 0 n2, doc (showString ","), prt 0 n3, doc (showString ")")])

instance Print AbsHashedLang.PiecewiseCase where
  prt i e = case e of
    AbsHashedLang.PiecewiseCase number exp -> prPrec i 0 (concatD [doc (showString "it"), doc (showString "<="), prt 0 number, doc (showString "->"), prt 0 exp])
    AbsHashedLang.PiecewiseFinalCase exp -> prPrec i 0 (concatD [doc (showString "otherwise"), doc (showString "->"), prt 0 exp])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [AbsHashedLang.PiecewiseCase] where
  prt = prtList

instance Print AbsHashedLang.Exp where
  prt i e = case e of
    AbsHashedLang.EPlus exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "+"), prt 1 exp2])
    AbsHashedLang.ERealImag exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "+:"), prt 1 exp2])
    AbsHashedLang.ESubtract exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "-"), prt 1 exp2])
    AbsHashedLang.EMul exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "*"), prt 2 exp2])
    AbsHashedLang.EDiv exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "/"), prt 2 exp2])
    AbsHashedLang.EScale exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "*."), prt 3 exp2])
    AbsHashedLang.EDot exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "<.>"), prt 3 exp2])
    AbsHashedLang.EPower exp n -> prPrec i 3 (concatD [prt 3 exp, doc (showString "^"), prt 0 n])
    AbsHashedLang.EFun pident exp -> prPrec i 4 (concatD [prt 0 pident, prt 5 exp])
    AbsHashedLang.ERotate rotateamount exp -> prPrec i 4 (concatD [doc (showString "rotate"), prt 0 rotateamount, prt 5 exp])
    AbsHashedLang.ENum number -> prPrec i 5 (concatD [prt 0 number])
    AbsHashedLang.EIdent pident -> prPrec i 5 (concatD [prt 0 pident])
    AbsHashedLang.EPiecewise exp piecewisecases -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString ":"), doc (showString "{"), prt 0 piecewisecases, doc (showString "}")])

