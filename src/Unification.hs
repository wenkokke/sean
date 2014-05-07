module Unification where

import Base (Name,Type (..),freeTypeVars)
import Error (Error,isError)
import Substitution (TySubst (..),apply)
import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (mapExceptT,throwE)
import Control.Monad.Supply (supply)
import Text.Printf (PrintfArg,printf)



-- * Unification with context of expression

unifyIn :: PrintfArg a => Type -> Type -> a -> Error [TySubst]
unifyIn t1 t2 c = mapExcept (\e -> printf "%s in %s" e c) (unify t1 t2)

mapExcept :: (String -> String) -> Error a -> Error a
mapExcept f = mapExceptT (\s -> mapLeft f <$> s)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right



-- * Unification algorithm

unify :: Type -> Type -> Error [TySubst]
unify (TyCon n1) (TyCon n2) =
  if n1 /= n2 then cannotUnify n1 n2 else return []
unify (TyArr a1 b1) (TyArr a2 b2) =
  do s1 <- unify a1 a2
     s2 <- unify (apply s1 b1) (apply s1 b2)
     return (s2 ++ s1)
unify t (TyVar n) = occursCheck n t
unify (TyVar n) t = occursCheck n t
unify t1 t2 = cannotUnify (show t1) (show t2)

unifiable :: Type -> Type -> Bool
unifiable t1 t2 = not (isError (unify t1 t2))

cannotUnify :: (PrintfArg t1, PrintfArg t2) => t1 -> t2 -> Error a
cannotUnify t1 t2 = throwE (printf "Cannot unify %s and %s" t1 t2)

occursCheck :: Name -> Type -> Error [TySubst]
occursCheck n t
  | n `occurs` t = throwE (printf "%s occurs in %s" n (show t))
  | otherwise    = return [TySubst n t]

occurs :: Name -> Type -> Bool
occurs n t = n `elem` freeTypeVars t

freshTyVar :: Error Type
freshTyVar = return . TyVar =<< lift supply
