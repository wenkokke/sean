{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SeAn.Lexicon.Base where

import Text.Printf (printf)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M (insert,lookup,empty,member)
import qualified Data.Maybe as M (fromMaybe)

-- * Basic abstract syntax tree

data Prog n
   = Prog { decls :: [Decl n] }
   deriving (Eq)

data Decl n
   = Decl n (Expr n)
   deriving (Eq)

data Expr n
  -- expressions
   = Con n
   | Var n
   | Abs n (Expr n)
   | App   (Expr n) (Expr n)
   | Let n (Expr n) (Expr n)

  -- holes and instantiation
   | Hole Type
   | Inst (Expr n) Name
   deriving (Eq)

data Type
   = TyCon TyName
   | TyVar TyName
   | TyArr Type Type
   deriving (Eq,Ord)

newtype ShortType
   = ShortType Type
   deriving (Eq,Ord)

type Name = String
type TyName = Name

-- * Utility methods for printing terms

instance IsName n => Show (Prog n) where
  show (Prog ds) = unlines (map show ds)

instance IsName n => Show (Decl n) where
  show (Decl n e) = printf "%s = %s" (collapse n) (show e)

instance Show ShortType where
  show (ShortType ty) = show ty
    where
    show (TyCon n)      = fmap toLower n
    show (TyVar n)      = n
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

instance IsName n => Show (Expr n) where
  show = show1 . mapNames collapse
    where
      show1 :: Expr Name -> String
      show1 (Var n) = n
      show1 (Con n) = n
      show1 (App (Con "NOT") e1)              = printf "~%s" (wrap1 e1)
      show1 (App (App (Con "EQUAL") e1) e2)   = printf "%s == %s"  (wrap2 e1) (wrap2 e2)
      show1 (App (App (Con "OR") e1) e2)      = printf "%s \\/ %s" (wrap2 e1) (wrap2 e2)
      show1 (App (App (Con "AND") e1) e2)     = printf "%s /\\ %s" (wrap2 e1) (wrap2 e2)
      show1 (App (App (Con "IMPLIES") e1) e2) = printf "%s => %s"  (wrap2 e1) (wrap2 e2)
      show1 (App (App (Con "EQUIV") e1) e2)   = printf "%s <=> %s" (wrap2 e1) (wrap2 e2)
      show1 (App (Con "FORALL") (Abs x e1))   = printf "!%s.%s" x (show1 e1)
      show1 (App (Con "EXISTS") (Abs x e1))   = printf "?%s.%s" x (show1 e1)
      show1 (App (Con "IOTA") (Abs x e1))     = printf "i%s.%s" x (show1 e1)
      show1 (Abs n e1)    = printf "\\%s.%s" n (show1 e1)
      show1 (App   e1 e2) = printf "%s %s" (wrap1 e1) (wrap1 e2)
      show1 (Let n e1 e2) = printf "let %s = %s in %s" n (show1 e1) (show1 e2)
      show1 (Hole t)      = printf "_:%s" (show (ShortType t))
      show1 (Inst e w)    = printf "%s @ %s" (wrap1 e) w

      wrap1 :: Expr Name -> String
      wrap1 e1@(App _ _) = printf "(%s)" (show1 e1)
      wrap1 e1           = wrap2 e1

      wrap2 :: Expr Name -> String
      wrap2 e1@(Var _)   = show1 e1
      wrap2 e1@(Con _)   = show1 e1
      wrap2 e1@(App _ _) = show1 e1
      wrap2 e1           = printf "(%s)" (show1 e1)

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

reserved :: Map Name Type
reserved
  = M.insert "TRUE"    (t)
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

isReservedName :: Name -> Bool
isReservedName n = M.member n reserved

-- |Maps constants to their type by their @Name@.
typeOf :: Name -> Type
typeOf n = M.fromMaybe (error $ "Undefined constant " ++ n) . M.lookup n $ reserved

-- |Maps functions over all names in a program.
class HasNames p where
  mapNames :: (a -> b) -> p a -> p b

instance HasNames Prog where
  mapNames f (Prog ds) = Prog (map (mapNames f) ds)

instance HasNames Decl where
  mapNames f (Decl n e) = Decl (f n) (mapNames f e)

instance HasNames Expr where
  mapNames f (Con n)       = Con (f n)
  mapNames f (Var n)       = Var (f n)
  mapNames f (Abs n e1)    = Abs (f n) (mapNames f e1)
  mapNames f (App e1 e2)   = App (mapNames f e1) (mapNames f e2)
  mapNames f (Let n e1 e2) = Let (f n) (mapNames f e1) (mapNames f e2)
  mapNames f (Hole t)      = Hole t
  mapNames f (Inst e w)    = Inst (mapNames f e) w


-- |Get all declared names from a program.
declaredNames :: Prog n -> [n]
declaredNames (Prog ds) = map (\(Decl n _) -> n) ds


-- * Base and complex names

type Label = [Int]

type family   NoType n :: *
type instance NoType Name = Name
type instance NoType (n,Label) = (NoType n,Label)
type instance NoType (n,Type) = NoType n

class (Ord n, Ord (NoType n)) => IsName n where
  base     :: n -> Name
  collapse :: n -> Name
  rename   :: Name -> n -> n
  retype   :: Type -> Name -> n
  noType   :: n -> NoType n

instance IsName Name where
  base     = id
  collapse = id
  rename   = const
  retype   = flip const
  noType   = id

instance IsName n => IsName (n,Label) where
  base              = base . fst
  collapse   (n,[]) = collapse n
  collapse   (n,ls) = collapse n ++ "#" ++ concatMap show ls
  rename   m (n,ls) = (rename m n,ls)
  retype      ty n  = (retype ty n,[])
  noType     (n,ls) = (noType n,ls)


instance IsName n => IsName (n,Type) where
  base              = base . fst
  collapse   (n,ty) = collapse n ++ ":" ++ show (ShortType ty)
  rename   m (n,ty) = (rename m n, ty)
  retype      ty n  = (retype ty n, ty)
  noType     (n,ty) = noType n


-- * Comparing declarations

-- |Check if two declarations define the same constant.
eqName :: Eq n => Decl n -> Decl n -> Bool
eqName (Decl m _) (Decl n _) = m == n
