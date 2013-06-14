module SeAn.W where

import SeAn.Base
import SeAn.Parsing (parseType)
import Text.Printf (printf)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Error (Error (..),ErrorT,runErrorT,throwError)
import Control.Monad.Supply (Supply,supply,evalSupply)
import Control.Monad.Trans (lift)

runW :: Expr -> Either TypeError Type
runW = runWWith mempty

runWWith :: TyEnv -> Expr -> Either TypeError Type
runWWith env e = fmap fst (evalSupply (runErrorT (w (env,e))) freshNames)

-- |Returns the set of free type variables in a type.
ftv :: Type -> Set Name
ftv (TyCon      _) = S.empty
ftv (TyVar      n) = S.singleton n
ftv (TyArr    a b) = S.union (ftv a) (ftv b)
  
type TySubst = Map Name Type

-- |Substitutes a type for a type variable in a type.
subst :: TySubst -> Type -> Type
subst m c@(TyCon n) = c
subst m v@(TyVar n) = M.findWithDefault v n m
subst m (TyArr f a) = TyArr (subst m f) (subst m a)

type TyEnv = Map Name Type

-- |Representation for possible errors in algorithm W.
data TypeError
  = UnknownConstant Name      -- ^ thrown when unknown constant is encountered
  | UnboundVariable Name      -- ^ thrown when unbound variable is encountered
  | OccursCheck     Name Type -- ^ thrown when occurs check in unify fails
  | CannotUnify     Type Type -- ^ thrown when types cannot be unified
  | OtherError      String    -- ^ stores miscellaneous errors
  | NoMsg                     -- ^ please don't be a jackass; don't use this
  deriving Eq

instance Error TypeError where
  noMsg       = NoMsg
  strMsg msg  = OtherError msg

instance Show TypeError where
  show (UnknownConstant n) = printf "Unknown constant %s" n
  show (UnboundVariable n) = printf "Unbound variable %s" n
  show (OccursCheck n  t1) = printf "Occurs check fails; %s occurs in %s" n (show t1)
  show (CannotUnify t1 t2) = printf "Cannot unify %s with %s" (show t1) (show t2)
  show (OtherError    msg) = msg
  show (NoMsg            ) = "nope"

type W a = ErrorT TypeError (Supply Name) a

-- |Occurs check for Robinson's unification algorithm.
occurs :: Name -> Type -> Bool
occurs n t = S.member n (ftv t)

-- |Unification as per Robinson's unification algorithm.
u :: Type -> Type -> W TySubst
u t1@(TyCon a) t2@(TyCon b)
  | a == b        = return mempty
  | otherwise     = throwError (CannotUnify t1 t2)
u (TyArr a1 b1) (TyArr a2 b2) = do
                    s1 <- u a1 a2
                    s2 <- u (subst s1 b1) (subst s1 b2)
                    return (s2 <> s1)
u t1 (TyVar n)
  | n `occurs` t1 = throwError (OccursCheck n t1)
  | otherwise     = return (M.singleton n t1)
u (TyVar n) t2
  | n `occurs` t2 = throwError (OccursCheck n t2)
  | otherwise     = return (M.singleton n t2)
u t1 t2           = throwError (CannotUnify t1 t2)

-- |Generates a fresh type in the W monad.
fresh :: W Type
fresh = do x <- lift supply;
           return (TyVar x)
           
-- |An infinite stream of fresh names.
freshNames :: [Name]
freshNames = letters ++ numbers
  where
  letters = fmap (: []) ['a'..'z']
  numbers = fmap (('t' :) . show) [0..]

-- |An alias for in the insertion of substitutions.
(~>) :: Name -> Type -> TyEnv -> TyEnv
(~>) = M.insert
           
-- |Implementation of algorithm W.
w :: (TyEnv,Expr) -> W (Type,TySubst)
w (env,exp) = case exp of
  Con n       -> case M.lookup n lib of
                    Just  t -> return (t,mempty)
                    Nothing -> throwError (UnknownConstant n)
  
  Var n       -> case M.lookup n env of
                    Just  t -> return (t,mempty)
                    Nothing -> throwError (UnboundVariable n)
  
  Abs   x e   -> do a <- fresh;
                    (t1,s1) <- w ((x ~> a) env,e);
                    return (TyArr (subst s1 a) t1,s1)
  
  App f   e   -> do (t1,s1) <- w (env,f);
                    (t2,s2) <- w (fmap (subst s1) env,e);
                    a  <- fresh;
                    s3 <- u (subst s2 t1) (TyArr t2 a);
                    return (subst s3 a, mconcat [s3,s2,s1])
  
  Let x e1 e2 -> do (t1,s1) <- w (env,e1);
                    (t2,s2) <- w ((x ~> t1).fmap (subst s1) $ env,e2);
                    return (t2, s2<>s1)

-- |Standard library of functions mapped to their types.
lib :: Map Name Type
lib = M.insert "TRUE"    (parseType "T")
    $ M.insert "FALSE"   (parseType "T")
    $ M.insert "NOT"     (parseType "T -> T")
    $ M.insert "AND"     (parseType "T -> T -> T")
    $ M.insert "OR"      (parseType "T -> T -> T")
    $ M.insert "IMPLIES" (parseType "T -> T -> T")
    $ M.insert "EQ"      (parseType "E -> E -> T")
    $ M.insert "FORALL"  (parseType "(E -> T) -> T")
    $ M.insert "EXISTS"  (parseType "(E -> T) -> T")
    $ M.insert "IOTA"    (parseType "(E -> T) -> E")
    $ M.empty
