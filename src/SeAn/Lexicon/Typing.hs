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

import Control.Monad.Error (Error (..),ErrorT,runErrorT,throwError,catchError)
import Control.Monad.Supply (Supply,supply,evalSupply)
import Control.Monad.Trans (lift)
import Control.Applicative ((<$>))

type NeedsFreshNames a = Supply Name a
type WithTypeErrors  a = Either TypeError a
type WithErrors a      = Either ProgError a
type W a               = ErrorT ProgError (Supply Name) a

-- TODO
--   update `inferTypes` to keep a list of all indexed names, and to make
--   sure that any errors are only propagated if *all* definitions of that
--   function fail (in case of `+` indices) or never (in case of `*` indices)

-- |Runs algorithm W on a list of declarations, making each previous
--  declaration an available expression in the next.
inferTypes :: Prog -> WithErrors TyEnv
inferTypes (Prog decls) = refreshAllW (supplyFreshNamesW (foldl addType (return []) decls))
  where
  addType :: W TyEnv -> Decl-> W TyEnv
  addType env (Decl n exp)
    = do env <- env ;
         (t , _) <- inferType exp env ;
         return (env << (n , t))

-- |Implementation of algorithm W.
inferType :: Expr -> TyEnv -> W (Type , TySubst)
inferType exp env = case exp of

  Con n t     -> return (t , Nil)

  Var n       -> handleVar env n
  n :@: w     -> handleVar env n

  Abs x e     -> do a <- freshW;
                    (t1 , s1) <- inferType e $ env << (x , a);
                    return (TyArr (apply s1 a) t1 , s1)

  App f x     -> do (t1 , s1) <- inferType f $ env
                    (t2 , s2) <- inferType x $ applyEnv s1 env
                    a  <- freshW
                    s3 <- unifyW exp (apply s2 t1) (TyArr t2 a)
                    return (apply s3 a , fromList [s1,s2,s3])

  Let x e1 e2 -> do (t1 , s1) <- inferType e1 $ env
                    (t2 , s2) <- inferType e2 $ applyEnv s1 env << (x , t1)
                    return (t2 , fromList [s1,s2])

handleVar :: TyEnv -> Name -> W (Type, TySubst)
handleVar env n = case findByName n env of
  (t:_) -> return (t, Nil)
  [   ] -> throwError (UnboundVariable n)

-- |Lifting of `unify` to the inference monad.
unifyW :: Expr -> Type -> Type -> W TySubst
unifyW exp t1 t2 = case unify t1 t2 of
  Left  e -> throwError (TypeErrorIn exp e)
  Right s -> return s

-- |Lifting of `supplyFreshNames` to the inference monad.
supplyFreshNamesW :: W a -> WithErrors a
supplyFreshNamesW m = evalSupply (runErrorT m) freshNames

-- |Lifting of `refreshAll` to the inference monad.
refreshAllW :: WithErrors TyEnv -> WithErrors TyEnv
refreshAllW env = env >>= return . refreshAll


-- * Type Environments

type TyEnv = [(Name,Type)]

-- |Inserts an entry into a type environment.
(<<) :: TyEnv -> (Name,Type) -> TyEnv
(<<) env (n,t) = (n,t) : env

-- |Replaces an entry in a type environment (based on name equality).
(<<!) :: TyEnv -> (Name,Type) -> TyEnv
(<<!) env (n,t) = (n,t) : filter ((n /=) . fst) env

-- |Applies a function to all types in a type environment.
mapEnv :: (Type -> Type) -> TyEnv -> TyEnv
mapEnv f = map (\(n,t) -> (n,f t))

-- |Finds all entries for a given name in a type environment.
findByName :: Name -> TyEnv -> [Type]
findByName n = map snd . filter ((n ==) . fst)



-- * Fresh Names

-- |Generates a fresh type variables.
fresh :: NeedsFreshNames Type
fresh = do x <- supply; return (TyVar x)

-- |Lifting of `fresh` to the inference monad.
freshW :: W Type
freshW = lift fresh

-- |Provides an infinite stream of names to things in the @W@ monad,
--  reducing it to just an @Either@ value containing perhaps a TypeError.
supplyFreshNames :: NeedsFreshNames a -> a
supplyFreshNames m = evalSupply m freshNames

-- |Simply stream of fresh names.
freshNames :: [Name]
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
refreshAll :: TyEnv -> TyEnv
refreshAll = mapEnv refresh



-- * Type Substitutions

data TySubst
  = Nil
  | Snoc Name Type TySubst
  deriving (Eq,Show)

-- |Performs a single substitution.
for :: Name -> Type -> Type -> Type
for x t' c@(TyCon _) = c
for x t' v@(TyVar y) = if x == y then t' else v
for x t' (TyArr a b) = TyArr (for x t' a) (for x t' b)

-- |Applies a substitution to a type.
apply :: TySubst -> Type -> Type
apply Nil t = t
apply (Snoc x t' s) t = for x t' (apply s t)

-- |Applies a substitution to a type environment.
applyEnv :: TySubst -> TyEnv -> TyEnv
applyEnv s env = mapEnv (apply s) env

-- |An alias for the creation of substitutions.
(~>) :: Name -> Type -> TySubst
(~>) x t' = Snoc x t' Nil

-- |An alias for substitution concatination.
andThen :: TySubst -> TySubst -> TySubst
andThen Nil s2 = s2
andThen (Snoc x t' s1) s2 = Snoc x t' (s1 `andThen` s2)

-- |An alias for substitution sequencing.
fromList :: [TySubst] -> TySubst
fromList = foldl andThen Nil



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
occurs :: Name -> Type -> Bool
occurs n t = n `elem` (ftv t)

-- |Returns the set of free type variables in a type.
ftv :: Type -> [Name]
ftv (TyCon   _) = [ ]
ftv (TyVar   n) = [n]
ftv (TyArr a b) = L.union (ftv a) (ftv b)



-- * Errors in Type Unification

-- |Representation for possible errors in unification.
data TypeError
  = OccursCheck   Name Type -- ^ thrown when occurs check in unify fails
  | CannotUnify   Type Type -- ^ thrown when types cannot be unified
  | MiscTypeError String    -- ^ stores miscellaneous errors
  deriving Eq

instance Error TypeError where
  strMsg msg = MiscTypeError msg

instance Show TypeError where
  show (OccursCheck n  t1) = printf "Occurs check fails; '%s' occurs in '%s'" n (show t1)
  show (CannotUnify t1 t2) = printf "Cannot unify '%s' with '%s'" (show t1) (show t2)
  show (MiscTypeError msg) = msg



-- * Errors in Type Inference

-- |Representation for possible errors in algorithm W.
data ProgError
  = UnknownConstant Name           -- ^ thrown when unknown constant is encountered
  | UnboundVariable Name           -- ^ thrown when unbound variable is encountered
  | TypeErrorIn     Expr TypeError -- ^ thrown when types of expressions cannot be unified
  | MiscProgError   String         -- ^ stores miscellaneous errors
  deriving Eq

instance Error ProgError where
  strMsg msg = MiscProgError msg

instance Show ProgError where
  show (UnknownConstant n)     = printf "Unknown constant '%s'" n
  show (UnboundVariable n)     = printf "Unbound variable '%s'" n
  show (TypeErrorIn exp err) = printf "Type error in %s; %s" (show exp) (show err)
  show (MiscProgError msg)     = msg
