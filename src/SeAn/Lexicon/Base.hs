{-# LANGUAGE FlexibleInstances #-}
module SeAn.Lexicon.Base where

import Text.Printf (printf)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M (insert,lookup,empty)
import qualified Data.Maybe as M (fromMaybe)

-- * Basic abstract syntax tree

data Prog i
   = Prog { decls :: [Decl i] }
   deriving (Eq)

data Decl i
   = Decl i (Expr i)
   deriving (Eq)

data Expr i
  -- expressions
   = Con i
   | Var i
   | Abs i (Expr i)
   | App   (Expr i) (Expr i)
   | Let i (Expr i) (Expr i)

  -- holes and instantiation
   | Hole Type
   | Inst i Name
   deriving (Eq)

data Type
   = TyCon Name
   | TyVar Name
   | TyArr Type Type
   deriving (Eq)

newtype ShortType
   = ShortType Type

type Name = String

-- * Utility methods for printing terms

instance Show ShortType where
  show (ShortType ty) = show ty
    where
    show (TyCon n)      = fmap toLower n
    show (TyArr a b)    = printf "%s%s" (wrap a) (wrap b)
    wrap ty@(TyArr _ _) = printf "(%s)" (show ty)
    wrap ty             = show ty

instance Show Type where
  show (TyCon n)   = n
  show (TyVar n)   = n
  show (TyArr a b) = printf "%s -> %s" (wrap a) (show b)
    where
    wrap ty@(TyArr _ _) = printf "(%s)" (show ty)
    wrap ty             = show ty

instance Show (Expr Name) where
  show (Var n) = show n
  show (App (Con "NOT") e1)             = printf "~%s" (wrapApp e1)
  show (App (App (Con "EQUAL") e1) e2)  = printf "%s == %s"  (wrapBin e1) (wrapBin e2)
  show (App (App (Con "OR") e1) e2)     = printf "%s \\/ %s" (wrapBin e1) (wrapBin e2)
  show (App (App (Con "AND") e1) e2)    = printf "%s /\\ %s" (wrapBin e1) (wrapBin e2)
  show (App (App (Con "IMPL") e1) e2)   = printf "%s => %s"  (wrapBin e1) (wrapBin e2)
  show (App (App (Con "EQUIV") e1) e2)  = printf "%s <=> %s" (wrapBin e1) (wrapBin e2)
  show (App (Con "FORALL") (Abs x e1))  = printf "!%s.%s" x (show e1)
  show (App (Con "EXISTS") (Abs x e1))  = printf "?%s.%s" x (show e1)
  show (App (Con "IOTA") (Abs x e1))    = printf "i%s.%s" x (show e1)
  show (Abs n e1)    = printf "\\%s.%s" n (show e1)
  show (App   e1 e2) = printf "%s %s" (wrapApp e1) (wrapApp e2)
  show (Let n e1 e2) = printf "let %s = %s in %s" n (show e1) (show e2)
  show (Hole t)      = printf "_:%s" (show (ShortType t))
  show (Inst n w)    = printf "%s @ %s" n w

wrapApp :: Expr Name -> String
wrapApp e1@(App _ _) = printf "(%s)" (show e1)
wrapApp e1           = wrapBin e1

wrapBin :: Expr Name -> String
wrapBin e1@(Var _)   = show e1
wrapBin e1@(Con _)   = show e1
wrapBin e1@(App _ _) = show e1
wrapBin e1           = printf "(%s)" (show e1)

-- * Utility methods for construction of complex terms

-- |Constructs a known constant from name.
con n = Con n

-- |Constructs an N-ary lambda abstraction.
abs xs e = foldr Abs e xs

-- |Constructs an N-ary universal quantifier.
univ = quant "FORALL"

-- |Constructs an N-ary existential quantifier.
exis = quant "EXISTS"

-- |Constructs an N-ary iota application.
iota = quant "IOTA"

-- |Constructs a quanfication based on a known quantifier.
quant n xs e = foldr ((App (con n).).Abs) e xs

-- |Constructs a definition tuple.
decl n xs e = Decl n (foldr Abs e xs)

-- |Constructs let bindings with multiple definitions.
letn decls e = foldr (\(Decl n e) -> Let n e) e decls

{-|
  The binding priority convention which is almost universally used
  for the connectives of propositional calculus is:

  (1): ~ binds more tightly than \/ and /\
  (2): \/ and /\ bind more tightly than -> and <-
  (3): -> and <- bind more tightly than <->

|-}

-- |Constructs a boolean negation.
not e = App (con "NOT") e

-- |Constructs a binary operator.
bin op e1 e2 = App (App (con op) e1) e2
nib op       = flip (bin op)

-- |Map of binary operators and their semantics.
bins =
  [ [("==" , bin "EQUAL")]
  , [("\\/", bin "OR"   ), ("/\\", bin "AND" )]
  , [("=>" , bin "IMPLIES" ), ("<=" , nib "IMPLIES")]
  , [("<=>", bin "IFF")]
  ]

-- |Maps constants to their type by their @Name@.
typeOf :: Name -> Type
typeOf n = M.fromMaybe (error $ "Undefined constant " ++ n)
         . M.lookup n
         $ M.insert "TRUE"    (t)
         $ M.insert "FALSE"   (t)
         $ M.insert "NOT"     (t ~> t)
         $ M.insert "AND"     (t ~> t ~> t)
         $ M.insert "OR"      (t ~> t ~> t)
         $ M.insert "IMPLIES" (t ~> t ~> t)
         $ M.insert "IFF"     (t ~> t ~> t)
         $ M.insert "EQ"      (e ~> e ~> t)
         $ M.insert "FORALL"  ((e ~> t) ~> t)
         $ M.insert "EXISTS"  ((e ~> t) ~> t)
         $ M.insert "IOTA"    ((e ~> t) ~> e)
         $ M.empty
         where
         infixr 4 ~>
         ((~>),e,t) = (TyArr, TyCon "e", TyCon "t")
