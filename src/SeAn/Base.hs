module SeAn.Base where

import Text.Printf (printf)

-- * Basic abstract syntax tree

data Prog
  = Prog [Decl]
  deriving (Eq,Show)
          
data Decl
  = Expr Name Expr
  deriving (Eq,Show)
  
data Type
  = TyCon Name
  | TyVar Name
  | TyArr Type Type
  deriving (Eq)

data Expr
  = Con Name
  | Var Name
  | Abs Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  deriving (Eq)

type Name = String

-- * Utility methods for printing terms

instance Show Type where
  show (TyCon n  ) = n
  show (TyVar n  ) = n
  show (TyArr a b) = printf "%s -> %s" (wrap a) (wrap b)
    where
    wrap ty@(TyArr _ _) = printf "(%s)" (show ty)
    wrap ty             = show ty
    
instance Show Expr where
  show (Var n) = n
  show (Con n) = n
  show (App (Con "NOT") e1) = printf "~%s" (wrapf e1)
  show (App (App (Con "EQUAL") e1) e2) = printf "%s == %s"  (wrap e1) (wrap e2)
  show (App (App (Con "OR"   ) e1) e2) = printf "%s \\/ %s" (wrap e1) (wrap e2)
  show (App (App (Con "AND"  ) e1) e2) = printf "%s /\\ %s" (wrap e1) (wrap e2)
  show (App (App (Con "IMPL" ) e1) e2) = printf "%s -> %s"  (wrap e1) (wrap e2)
  show (App (App (Con "EQUIV") e1) e2) = printf "%s <-> %s" (wrap e1) (wrap e2)
  show (App (Con "FORALL") (Abs x e1)) = printf "!%s.%s" x (show e1)
  show (App (Con "EXISTS") (Abs x e1)) = printf "?%s.%s" x (show e1)
  show (App (Con "IOTA"  ) (Abs x e1)) = printf "i%s.%s" x (show e1)
  show (Abs x e1) = printf "\\%s.%s" x (show e1)
  show (App e1 e2) = printf "%s %s" (wrapf e1) (wrapf e2)
  
wrap :: Expr -> String
wrap e1@(Var _  ) = show e1
wrap e1@(Con _  ) = show e1
wrap e1@(App _ _) = show e1
wrap e1           = printf "(%s)" (show e1)

wrapf :: Expr -> String
wrapf e1@(App _ _) = printf "(%s)" (show e1)
wrapf e1           = wrap e1

-- * Utility methods for construction of complex terms

-- |Constructs an N-ary lambda abstraction.
abs xs e = foldr Abs e xs

-- |Constructs an N-ary universal quantifier.
univ xs e = foldr ((App (Con "FORALL").).Abs) e xs

-- |Constructs an N-ary existential quantifier.
exis xs e = foldr ((App (Con "EXISTS").).Abs) e xs

-- |Constructs an N-ary iota application.
iota xs e = foldr ((App (Con "IOTA"  ).).Abs) e xs

-- |Constructs a definition tuple.
def = (,)

-- |Constructs let bindings with multiple definitions.
letn defs e = foldr (uncurry Let) e defs

{-| 
  The binding priority convention which is almost universally used
  for the connectives of propositional calculus is:
  
  (1): ~ binds more tightly than \/ and /\
  (2): \/ and /\ bind more tightly than -> and <-
  (3): -> and <- bind more tightly than <->
  
|-}

-- |Constructs a boolean negation.
not e = App (Con "NOT") e
  
-- |Constructs a binary operator.
bin op e1 e2 = App (App (Con op) e1) e2
nib op       = flip (bin op)
  
-- |Map of binary operators and their semantics.
bins =
  [ [("==" , bin "EQUAL")]
  , [("\\/", bin "OR"   ),("/\\", bin "AND" )]
  , [("->" , bin "IMPL" ),("<-" , nib "IMPL")]
  , [("<->", bin "EQUIV")]
  ]
