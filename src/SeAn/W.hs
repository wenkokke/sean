module SeAn.W where

import SeAn.Base
import SeAn.Parsing (parseType)
import Text.Printf (printf)

import Data.Monoid
import Data.Traversable (forM)

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Traversable as M
import qualified Data.List as L
import qualified Data.Traversable as L

import Control.Monad.Error (Error (..),ErrorT,runErrorT,throwError,catchError)
import Control.Monad.Supply (Supply,supply,evalSupply)
import Control.Monad.Trans (lift)
import Control.Applicative ((<$>))

-- |Runs algorithm W on a list of declarations, making each previous
--  declaration an available expression in the next.
runW :: [Decl] -> Either TypeError TyEnv
runW = refreshAll . withFreshNames . foldl addDecl (return mempty)
  where
  addDecl :: W TyEnv -> Decl-> W TyEnv
  addDecl env (Decl x e) = do env <- env;
                              (t,_) <- w (env,e);
                              return (M.insert x t env)

-- |Returns the set of free type variables in a type.
ftv :: Type -> [Name]
ftv (TyCon   _) = [ ]
ftv (TyVar   n) = [n]
ftv (TyArr a b) = L.union (ftv a) (ftv b)
  
type TySubst = Map Name Type

-- |Substitutes a type for a type variable in a type.
subst :: TySubst -> Type -> Type
subst m c@(TyCon n) = c
subst m v@(TyVar n) = M.findWithDefault v n m
subst m (TyArr a b) = TyArr (subst m a) (subst m b)

type TyEnv = Map Name Type

-- |Representation for possible errors in algorithm W.
data TypeError
  = UnknownConstant Name                -- ^ thrown when unknown constant is encountered
  | UnboundVariable Name                -- ^ thrown when unbound variable is encountered
  | OccursCheck     Name Type           -- ^ thrown when occurs check in unify fails
  | CannotUnify     Type Type           -- ^ thrown when types cannot be unified
  | CannotUnifyIn   Type Type Expr Expr -- ^ thrown when types of expressions cannot be unified
  | OtherError      String              -- ^ stores miscellaneous errors
  deriving Eq

instance Error TypeError where
  strMsg msg = OtherError msg

instance Show TypeError where
  show (UnknownConstant n)
    = printf "Unknown constant '%s'" n
  show (UnboundVariable n)
    = printf "Unbound variable '%s'" n
  show (OccursCheck n  t1)
    = printf "Occurs check fails; '%s' occurs in '%s'" n (show t1)
  show (CannotUnify t1 t2)
    = printf "Cannot unify '%s' with '%s'" (show t1) (show t2)
  show (CannotUnifyIn t1 t2 e1 e2)
    = printf "Cannot unify '%s' with '%s' in (%s %s)" (show t1) (show t2) (show e1) (show e2)
  show (OtherError msg) = msg

type W a = ErrorT TypeError (Supply Name) a

-- |Occurs check for Robinson's unification algorithm.
occurs :: Name -> Type -> Bool
occurs n t = n `elem` (ftv t)

-- |Unification as per Robinson's unification algorithm.
unify :: Type -> Type -> W TySubst
unify t1@(TyCon a) t2@(TyCon b)
  | a == b        = return mempty
  | otherwise     = throwError (CannotUnify t1 t2)
unify (TyArr a1 b1) (TyArr a2 b2)
                  = do
                    s1 <- unify a1 a2
                    s2 <- unify (subst s1 b1) (subst s1 b2)
                    return (s2 <> s1)
unify t1 (TyVar n)
  | n `occurs` t1 = throwError (OccursCheck n t1)
  | otherwise     = return (M.singleton n t1)
unify (TyVar n) t2
  | n `occurs` t2 = throwError (OccursCheck n t2)
  | otherwise     = return (M.singleton n t2)
unify t1 t2       = throwError (CannotUnify t1 t2)

-- |An alias for in the insertion of substitutions.
(~>) :: Name -> Type -> TyEnv -> TyEnv
(~>) = M.insert

-- |Implementation of algorithm W.
w :: (TyEnv,Expr) -> W (Type , TySubst)
w (env,exp) = case exp of
  Con n t     -> return (t , mempty)
  Var n       -> case M.lookup n env of
                    Just  t -> return (t , mempty)
                    Nothing -> throwError (UnboundVariable n)
  
  Abs   x e   -> do a <- fresh;
                    (t1 , s1) <- w ((x ~> a) env, e);
                    return (TyArr (subst s1 a) t1 , s1)
  
  App f   e   -> do (t1 , s1) <- w (env , f);
                    (t2 , s2) <- w (subst s1 <$> env,e);
                    a <- fresh;
                    
                    let s3_no_info = unify (subst s2 t1) (TyArr t2 a)
                    let s3_with_info = handleAddExpr f e s3_no_info
                    
                    s3 <- s3_with_info
                    return (subst s3 a , s3 <> (subst s3 <$> (s1 <> s2)))
  
  Let x e1 e2 -> do (t1 , s1) <- w (env , e1);
                    (t2 , s2) <- w ((x ~> t1) (subst s1 <$> env) , e2);
                    return (t2 , s2 <> s1)

handleAddExpr :: Expr -> Expr -> W a -> W a
handleAddExpr e1 e2 m = m `catchError` addExpr e1 e2

addExpr :: Expr -> Expr -> TypeError -> W a
addExpr e1 e2 (CannotUnify t1 t2) = throwError (CannotUnifyIn t1 t2 e1 e2)

-- * Fresh Names

-- |Generates a fresh type in the W monad.
fresh :: W Type
fresh = do x <- lift supply;
           return (TyVar x)

-- |Provides an infinite stream of names to things in the @W@ monad,
--  reducing it to just an @Either@ value containing perhaps a TypeError.
withFreshNames :: W a -> Either TypeError a
withFreshNames x = evalSupply (runErrorT x) freshNames
  where
  freshNames = letters ++ numbers
    where
    letters = fmap (: []) (['A'..'D'] ++ ['F'..'S'] ++ ['U'..'Z'])
    numbers = fmap (('T' :) . show) [0..]

-- |Replaces every type variable with a fresh one.
refresh :: Type -> W Type
refresh t1 = do subs <- forM (ftv t1)
                        $ \a ->
                          do b <- fresh;
                             return (M.singleton a b)
                return (subst (mconcat subs) t1)
                              
-- |Refreshes all entries in a type environment.
refreshAll :: Either TypeError TyEnv -> Either TypeError TyEnv
refreshAll env = do env <- env;
                    M.mapM (withFreshNames . refresh) env