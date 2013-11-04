{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module SeAn.Lexicon.Typing where

import SeAn.Lexicon.Base
import SeAn.Lexicon.Parsing (parseType)

import Text.Printf (printf)

import Data.Monoid
import Data.Traversable (forM)

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Traversable as M
import qualified Data.List as L
import qualified Data.Traversable as L
import Data.Either (lefts,rights)

import Control.Monad.Error (Error (..),ErrorT,runErrorT,throwError,catchError)
import Control.Monad.Supply (Supply,supply,evalSupply)
import Control.Monad.Trans (lift)
import Control.Applicative ((<$>))

type NeedsFreshNames a = Supply TyName a
type WithTypeErrors a = Either TypeError a
type WithErrors a n = Either (ProgError n) a
type W a n = ErrorT (ProgError n) (Supply TyName) a

-- |Runs algorithm W on a list of declarations, making each previous
--  declaration an available expression in the next.
inferTypes :: IsName n => Prog n -> WithErrors (Env n) n
inferTypes (Prog ds) = refreshAllW (supplyFreshNamesW (foldl infGrp (return emptyEnv) grps))
  where
    grps = L.groupBy eqName ds

-- |Lifting of `supplyFreshNames` to the inference monad.
supplyFreshNamesW :: W a n -> WithErrors a n
supplyFreshNamesW m = evalSupply (runErrorT m) freshNames

-- |Stream of simple names.
freshNames :: [TyName]
freshNames = letters ++ numbers
  where
  letters    = fmap (: []) (['A'..'D'] ++ ['F'..'S'] ++ ['U'..'Z'])
  numbers    = fmap (('T' :) . show) [0..]

-- |Lifting of `refreshAll` to the inference monad.
refreshAllW :: WithErrors (Env n) n -> WithErrors (Env n) n
refreshAllW env = env >>= return . refreshAll

-- |Infers the types for a group of declarations, possibly failing.
infGrp :: IsName n => W (Env n) n -> [Decl n] -> W (Env n) n
infGrp env grp = do
  env <- env
  let infs = map (infDecl env) grp
  let (errs,dcls) = L.partition isError infs
  foldl (<<!) (return env)
    $ if L.null dcls then errs else dcls

-- |Infers the type of a declaration, possibly failing.
infDecl :: IsName n => Env n -> Decl n -> W (n,Type) n
infDecl env (Decl n e) = inferType e env >>= return . (n,) . fst

-- |Checks if a value in the W monad is an error.
isError :: W a n -> Bool
isError w = case supplyFreshNamesW w of
  Left  _ -> True
  Right _ -> False

-- |Implementation of algorithm W.
inferType :: IsName n => Expr n -> Env n -> W (Type,TySubst) n
inferType exp env = case exp of

  Con n       -> return (typeOf . base $ n , Nil)

  Var n       -> case findByName n env of
                  Just t  -> return (t, Nil)
                  Nothing -> throwError (UnboundVariable n)

  Abs x e     -> do a <- freshW;
                    (t1 , s1) <- inferType e $ env << (x , a)
                    return (TyArr (apply s1 a) t1 , s1)

  App f x     -> do (t1 , s1) <- inferType f $ env
                    (t2 , s2) <- inferType x $ applyEnv s1 env
                    a  <- freshW
                    s3 <- unifyW exp (apply s2 t1) (TyArr t2 a)
                    return (apply s3 a , fromList [s3,s2,s1])

  Let x e1 e2 -> do (t1 , s1) <- inferType e1 $ env
                    (t2 , s2) <- inferType e2 $ applyEnv s1 env << (x , t1)
                    return (t2 , fromList [s2,s1])

  Hole t      -> return (t, Nil)

  Inst n w    -> case findByName n env of
                  Just t  -> return (t, Nil)
                  Nothing -> throwError (UnboundVariable n)

-- |Lifting of `unify` to the inference monad.
unifyW :: Expr n -> Type -> Type -> W TySubst n
unifyW exp t1 t2 = case unify t1 t2 of
  Left  e -> throwError (TypeErrorIn exp e)
  Right s -> return s

-- * Type Environments

type Env n = Map n Type

-- |Inserts an entry into a type environment.
(<<) :: IsName n => Env n -> (n,Type) -> Env n
(<<) env (n,t) = M.insert n t env

-- |Replaces an entry in a type environment (based on name equality).
(<<!) :: IsName n => W (Env n) n -> W (n,Type) n -> W (Env n) n
(<<!) env w = do env <- env; (n,t) <- w; return $ env << (n,t)

-- |Applies a function to all types in a type environment.
mapEnv :: (Type -> Type) -> Env n -> Env n
mapEnv = M.map

-- |Finds all entries for a given name in a type environment.
findByName :: IsName n => n -> Env n -> Maybe Type
findByName = M.lookup

-- |The empty environment.
emptyEnv :: (Env n)
emptyEnv = M.empty


-- * Fresh ns

-- |Generates a fresh type variables.
fresh :: NeedsFreshNames Type
fresh = do x <- supply; return (TyVar x)

-- |Lifting of `fresh` to the inference monad.
freshW :: W Type n
freshW = lift fresh

-- |Provides an infinite stream of names to things in the @W@ monad,
--  reducing it to just an @Either@ value containing perhaps a TypeError.
supplyFreshNames :: NeedsFreshNames a -> a
supplyFreshNames m = evalSupply m freshNames

-- |Replaces every type variable with a fresh one.
refresh :: Type -> Type
refresh = supplyFreshNames . lazyRefresh
  where
  -- |Lazily replace every type variable with a fresh one.
  lazyRefresh :: Type -> NeedsFreshNames Type
  lazyRefresh t = do subs <- forM (ftv t)
                             $ \a ->
                               do b <- fresh;
                                  return (a ~> b)
                     return (apply (fromList subs) t)

-- |Refreshes all entries in a type environment.
refreshAll :: Env n -> Env n
refreshAll = mapEnv refresh



-- * Type Substitutions

data TySubst
  = Nil | Snoc TyName Type TySubst
  deriving (Eq,Show)

-- |Performs a single substitution.
for :: Type -> Name -> Type -> Type
for t' x c@(TyCon _) = c
for t' x v@(TyVar y) = if x == y then t' else v
for t' x (TyArr a b) = TyArr (for t' x a) (for t' x b)

-- |Applies a substitution to a type.
apply :: TySubst -> Type -> Type
apply Nil t = t
apply (Snoc x t' s) t = (for t' x) (apply s t)

-- |Applies a substitution to a type environment.
applyEnv :: TySubst -> Env n -> Env n
applyEnv s env = mapEnv (apply s) env

-- |An alias for the creation of substitutions.
(~>) :: TyName -> Type -> TySubst
(~>) x t' = Snoc x t' Nil

-- |An alias for substitution concatination.
andThen :: TySubst -> TySubst -> TySubst
andThen Nil s2 = s2
andThen (Snoc x t' s1) s2 = Snoc x t' (s1 `andThen` s2)

-- |An alias for substitution sequencing.
fromList :: [TySubst] -> TySubst
fromList = foldr andThen Nil



-- * Type Unifications

-- |Unification as per Robinson's unification algorithm.
unify :: Type -> Type -> WithTypeErrors TySubst
unify t1@(TyCon a) t2@(TyCon b)
    | a == b         = return Nil
    | otherwise      = throwError (CannotUnify t1 t2)
unify (TyArr a1 b1) (TyArr a2 b2)
                     = do
                       s1 <- unify a1 a2
                       s2 <- unify (apply s1 b1) (apply s1 b2)
                       return (s1 `andThen` s2)
unify ty (TyVar x)
    | x `occurs` ty = throwError (OccursCheck x ty)
    | otherwise     = return (x ~> ty)
unify (TyVar x) ty
    | x `occurs` ty = throwError (OccursCheck x ty)
    | otherwise     = return (x ~> ty)
unify t1 t2         = throwError (CannotUnify t1 t2)

-- |Occurs check for Robinson's unification algorithm.
occurs :: TyName -> Type -> Bool
occurs n t = n `elem` (ftv t)

-- |Returns the set of free type variables in a type.
ftv :: Type -> [TyName]
ftv (TyCon   _) = [ ]
ftv (TyVar   n) = [n]
ftv (TyArr a b) = L.union (ftv a) (ftv b)



-- * Errors in Type Unification

-- |Representation for possible errors in unification.
data TypeError
  = OccursCheck TyName Type -- ^ thrown when occurs check in unify fails
  | CannotUnify Type Type   -- ^ thrown when types cannot be unified
  | MiscTypeError String    -- ^ stores miscellaneous errors
  deriving Eq

instance Error TypeError where
  strMsg msg = MiscTypeError msg

instance Show TypeError where
  show (OccursCheck n t1)  = printf "Occurs check fails; '%s' occurs in '%s'" n (show t1)
  show (CannotUnify t1 t2) = printf "Cannot unify '%s' with '%s'" (show t1) (show t2)
  show (MiscTypeError msg) = msg



-- * Errors in Type Inference

-- |Representation for possible errors in algorithm W.
data ProgError n
  = UnknownConstant n              -- ^ thrown when unknown constant is encountered
  | UnboundVariable n              -- ^ thrown when unbound variable is encountered
  | TypeErrorIn (Expr n) TypeError -- ^ thrown when types of expressions cannot be unified
  | MiscProgError String           -- ^ stores miscellaneous errors
  deriving Eq

instance Error (ProgError n) where
  strMsg msg = MiscProgError msg

instance IsName n => Show (ProgError n) where
  show (UnknownConstant n)   = printf "Unknown constant '%s'" (collapse n)
  show (UnboundVariable n)   = printf "Unbound variable '%s'" (collapse n)
  show (TypeErrorIn exp err) = printf "Type error in %s; %s"
                                 (show . mapNames collapse $ exp) (show err)
  show (MiscProgError msg)   = msg
