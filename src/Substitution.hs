{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module Substitution where


import Base
import Data.Map (Map)
import qualified Data.Map as M (map)


class Apply f a where
  apply :: f -> a -> a

instance (Apply f a) => Apply [f] a where
  apply [] = id
  apply (f : fs) = apply f . apply fs

instance (Apply f a1, Apply f a2) => Apply f (a1 , a2) where
  apply f (x1 , x2) = (apply f x1 , apply f x2)

instance (Apply f a1, Apply f a2 , Apply f a3) => Apply f (a1 , a2 , a3) where
  apply f (x1 , x2 , x3) = (apply f x1 , apply f x2 , apply f x3)

instance (Apply f a) => Apply f [a] where
  apply f [] = []
  apply f (x : xs) = apply f x : apply f xs


-- * Type substitutions

data TySubst where
  TySubst :: Name -> Type -> TySubst

instance Apply TySubst Type where
  apply (TySubst n1 t) = subst
    where
      subst v@(TyVar n2) = if n1 == n2 then t else v
      subst (TyArr t1 t2) = TyArr (subst t1) (subst t2)
      subst c = c

instance Apply TySubst TyAnn where
  apply s = fmap (apply s)

instance Apply TySubst Expr where
  apply s (Var n t)        = Var n (apply s t)
  apply s (Abs n e t)      = Abs n (apply s e) (apply s t)
  apply s (App e1 e2 t)    = App (apply s e1) (apply s e2) (apply s t)
  apply s (Obj i t)        = Obj i (apply s t)
  apply s (Rel1 es t)      = Rel1 (apply s es) (apply s t)
  apply s (Rel2 es t)      = Rel2 (apply s es) (apply s t)
  apply s (Rel3 es t)      = Rel3 (apply s es) (apply s t)
  apply s (Plug c e t)     = Plug (apply s c) (apply s e) (apply s t)
  apply s (Hole t)         = Hole (apply s t)

instance Apply TySubst (Map Name Type) where
  apply s = M.map (apply s)



-- * Expression substitutions

data Subst where
  Subst :: Name -> Expr -> Subst

instance Apply Subst Expr where
  apply s@(Subst n1 r) = subst
    where
      subst v@(Var n2 _)       = if n1 == n2 then r else v
      subst a@(Abs n2 e t)     = if n1 == n2 then a else Abs n2 (subst e) t
      subst (App e1 e2 t)      = App (subst e1) (subst e2) t
      subst o@(Obj _ _)        = o
      subst (Rel1 es t)        = Rel1 (apply s es) t
      subst (Rel2 es t)        = Rel2 (apply s es) t
      subst (Rel3 es t)        = Rel3 (apply s es) t
      subst (Plug e c t)       = Plug (subst e) (apply s c) t
      subst h@(Hole _)         = h
