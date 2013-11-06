{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module SeAn.Lexicon.Typing
       (inferTypes,unify
       ,WithErrors,ProgError (..)
       ,WithTypeErrors,TypeError (..)) where

import SeAn.Lexicon.Base
import Text.Printf (printf)
import Data.Monoid
import Data.Traversable (forM)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Traversable as M
import qualified Data.List as L
import qualified Data.Traversable as L
import Data.Either (lefts,rights)
import Control.Arrow ((***))
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
inferTypes :: IsName n => Prog n -> WithErrors (Prog (n,Type)) n
inferTypes (Prog ds) = do
  (_,ds) <- supplyFreshNamesW (foldl inferGroupTypes initial groups)
  return $ Prog (reverse . map (mapNames (id *** refresh)) $ ds)
  where
    groups = L.groupBy eqName ds
    initial :: W (Env n, [Decl (n,Type)]) n
    initial = return (emptyEnv, [])


-- |Infers the types for a group of declarations, possibly failing.
inferGroupTypes :: IsName n
  => W (Env n,[Decl (n,Type)]) n -- ^ Initial result.
  -> [Decl n]                    -- ^ Next group use for type inference.
  -> W (Env n,[Decl (n,Type)]) n -- ^ Updated result.
inferGroupTypes rsl grp = do
  (env,ds) <- rsl
  let inferences = map (\d -> inferDeclType d env) grp
  let (errors,types) = L.partition isError inferences
  foldl updateW rsl
    $ if L.null types then errors else types
  where
    update (env,ds) d@(Decl (n,ty) e) = (env << (n,ty), d:ds)
    updateW rsl d = do rsl' <- rsl; d' <- d; return (update rsl' d')


-- |Infers the type of a declaration, possibly failing.
inferDeclType :: IsName n => Decl n -> Env n ->  W (Decl (n,Type)) n
inferDeclType (Decl n e) env = do
  (e',ty,_) <- inferExprType e env
  return (Decl (n,ty) e')

-- |Checks if a value in the W monad is an error.
isError :: W a n -> Bool
isError = either (const True) (const False) . supplyFreshNamesW

-- |Implementation of algorithm W.
inferExprType :: IsName n => Expr n -> Env n -> W (Expr (n,Type), Type, TySubst) n
inferExprType exp env = case exp of

  Con n       -> if isReservedName (base n)
                    then do let ty = typeOf (base n)
                            return (Con (n,ty), ty, Nil)
                    else case findByName n env of
                  Just ty -> return (Con (n,ty), ty, Nil)
                  Nothing -> throwError (UnboundVariable n)

  Var n       -> case findByName n env of
                  Just ty -> return (Var (n,ty), ty, Nil)
                  Nothing -> throwError (UnboundVariable n)

  Abs x e     -> do a1 <- freshW;
                    (e1, t1, s1) <- inferExprType e $ env << (x,a1)
                    let a2 = apply s1 a1
                    let e2 = apply s1 e1
                    return (Abs (x,a2) e2, TyArr a2 t1 , s1)

  App f x     -> do (f1, t1 , s1) <- inferExprType f $ env
                    (x1, t2 , s2) <- inferExprType x $ apply s1 env
                    a1 <- freshW
                    s3 <- unifyW exp (apply s2 t1) (TyArr t2 a1)
                    let a2 = apply s3 a1
                    let f2 = apply s3 f1
                    let x2 = apply s3 x1
                    return (App f2 x2, a2, fromList [s3,s2,s1])

  Let x e1 e2 -> do (e1', t1 , s1) <- inferExprType e1 $ env
                    (e2', t2 , s2) <- inferExprType e2 $ apply s1 env << (x , t1)
                    return (Let (x,t1) e1' e2', t2, fromList [s2,s1])

  Hole t      -> return (Hole t, t, Nil)

  Inst e w    -> do (e', t1, s1) <- inferExprType e $ env
                    return (Inst e' w, t1, Nil)

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


-- * Fresh names

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

-- |Lifting of `supplyFreshNames` to the inference monad.
supplyFreshNamesW :: W a n -> WithErrors a n
supplyFreshNamesW m = evalSupply (runErrorT m) freshNames

-- |Stream of simple names.
freshNames :: [TyName]
freshNames = letters ++ numbers
  where
  letters    = fmap (: []) (['A'..'D'] ++ ['F'..'S'] ++ ['U'..'Z'])
  numbers    = fmap (('T' :) . show) [0..]

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

class Subst a where
  apply :: TySubst -> a -> a

instance Subst Type where
  apply Nil t = t
  apply (Snoc x t' s) t = (for t' x) (apply s t)

instance Subst (Env n) where
  apply s = mapEnv (apply s)

instance Subst (Expr (n,Type)) where
  apply s = mapNames (\(n,ty) -> (n,apply s ty))

-- |Performs a single substitution.
for :: Type -> Name -> Type -> Type
for t' x c@(TyCon _) = c
for t' x v@(TyVar y) = if x == y then t' else v
for t' x (TyArr a b) = TyArr (for t' x a) (for t' x b)

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
