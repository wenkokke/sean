{-# LANGUAGE FlexibleInstances #-}
module SeAn.Base where

import Text.Printf (printf)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M (insert,lookup,empty)
import qualified Data.Maybe as M (fromJust)

-- * Basic abstract syntax tree

data Prog
  = Prog [Decl]
  deriving (Eq,Show)
          
data Decl
  = Decl Name Expr
  deriving (Eq,Show)
  
data Type
  = TyCon Name
  | TyVar Name
  | TyArr Type Type
  deriving (Eq)
  
newtype ShortType
  = ShortType Type

data Expr
  = Con Name Type
  | Var Name
  | Abs Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  deriving (Eq)

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
    
instance Show Expr where
  show (Var n) = n
  show (Con n t) = printf "%s:%s" n (show (ShortType t))
  show (App (Con "NOT" _) e1) = printf "~%s" (wrap_fun e1)
  show (App (App (Con "EQUAL" _) e1) e2) = printf "%s == %s"  (wrap_bin e1) (wrap_bin e2)
  show (App (App (Con "OR"    _) e1) e2) = printf "%s \\/ %s" (wrap_bin e1) (wrap_bin e2)
  show (App (App (Con "AND"   _) e1) e2) = printf "%s /\\ %s" (wrap_bin e1) (wrap_bin e2)
  show (App (App (Con "IMPL"  _) e1) e2) = printf "%s -> %s"  (wrap_bin e1) (wrap_bin e2)
  show (App (App (Con "EQUIV" _) e1) e2) = printf "%s <-> %s" (wrap_bin e1) (wrap_bin e2)
  show (App (Con "FORALL" _) (Abs x e1)) = printf "!%s.%s" x (show e1)
  show (App (Con "EXISTS" _) (Abs x e1)) = printf "?%s.%s" x (show e1)
  show (App (Con "IOTA"   _) (Abs x e1)) = printf "i%s.%s" x (show e1)
  show (Abs x  e1) = printf "\\%s.%s" x (show e1)
  show (App e1 e2) = printf "%s %s" (wrap_fun e1) (wrap_fun e2)
  show (Let x e1 e2) = printf "let %s = %s in %s" x (show e1) (show e2)

wrap_fun :: Expr -> String
wrap_fun e1@(App _ _) = printf "(%s)" (show e1)
wrap_fun e1           = wrap_bin e1
  
wrap_bin :: Expr -> String
wrap_bin e1@(Var _  ) = show e1
wrap_bin e1@(Con _ _) = show e1
wrap_bin e1@(App _ _) = show e1
wrap_bin e1           = printf "(%s)" (show e1)

-- * Utility methods for construction of complex terms

-- |Constructs a known constant from name.
con n = Con n (typeOf n)

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
  , [("\\/", bin "OR"   ),("/\\", bin "AND" )]
  , [("->" , bin "IMPL" ),("<-" , nib "IMPL")]
  , [("<->", bin "EQUIV")]
  ]

-- |Maps constants to their type by their @Name@.
typeOf :: Name -> Type
typeOf n = M.fromJust . M.lookup n
         $ M.insert "TRUE"    (t)
         $ M.insert "FALSE"   (t)
         $ M.insert "NOT"     (t ~> t)
         $ M.insert "AND"     (t ~> t ~> t)
         $ M.insert "OR"      (t ~> t ~> t)
         $ M.insert "IMPLIES" (t ~> t ~> t)
         $ M.insert "EQ"      (e ~> e ~> t)
         $ M.insert "FORALL"  ((e ~> t) ~> t)
         $ M.insert "EXISTS"  ((e ~> t) ~> t)
         $ M.insert "IOTA"    ((e ~> t) ~> e)
         $ M.empty
         where
         ((~>),e,t) = (TyArr, TyCon "T", TyCon "E")
