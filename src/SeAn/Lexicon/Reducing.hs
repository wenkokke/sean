module SeAn.Lexicon.Reducing (reduce) where

import SeAn.Lexicon.Base
import SeAn.Lexicon.Typing

import Text.Printf (printf)

import Control.Arrow ((***))
import Control.Exception
import Control.Monad.Error

import Data.List (elemIndex,partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

isAnnotation :: Decl n -> Bool
isAnnotation (Decl _ e) = go e
  where
    go :: Expr n -> Bool
    go e = case e of
      Con n       -> False
      Var n       -> False
      Abs n e     -> go e
      App   e1 e2 -> go e1 || go e2
      Let n e1 e2 -> go e1 || go e2
      Hole t      -> True
      Inst e a    -> False

-- * Reduction functions

tuple :: Decl n -> (n,Expr n)
tuple (Decl n e) = (n,e)

reduce :: IsName n => Prog n -> Either (ReductionError n) (Prog n)
reduce p@(Prog ds) = do
  env <- foldM reduceDecl (toEnv as) (reverse es)
  return $ Prog (M.elems env)
  where
    (as,es) = partition isAnnotation ds

toEnv :: IsName n => [Decl n] -> Env n
toEnv = M.fromList . map (\d@(Decl n _) -> (noType n,d))

reduceDecl :: IsName n => Env n -> Decl n -> Either (ReductionError n) (Env n)
reduceDecl env (Decl n e0) = do
  e1 <- reduceExpr env e0
  return $ M.insert (noType n) (Decl n e1) env

reduceExpr :: IsName n => Env n -> Expr n -> Either (ReductionError n) (Expr n)
reduceExpr env e = expand (env, Nothing) e `catchError` addInfo e

type Env n = Map (NoType n) (Decl n)

findByName :: IsName n => Env n -> n -> Either (ReductionError n) (Expr n)
findByName ds n = maybe ifNothing ifJust maybeExpr
  where
    ifJust    = return
    ifNothing = throwError (UnknownName n (Con n))
    maybeExpr = do (Decl _ e) <- M.lookup (noType n) ds; return e

-- |Expands lambda terms by recursive lookup in an environment.
expand :: IsName n => (Env n, Maybe Name) -> Expr n -> Either (ReductionError n) (Expr n)
expand env@(ds,m) exp = case exp of

  c@(Con n)   -> if isReservedName (base n)
                 then do return c
                 else do e1 <- findByName ds n
                         expand env e1

  v@(Var _)   -> do return v

  Abs x e1    -> do e1 <- expand env e1
                    return $ Abs x e1

  App   e1 e2 -> do e1 <- expand env e1
                    e2 <- expand env e2
                    return $ App e1 e2

  Let x e1 e2 -> do e1 <- expand env e1
                    e2 <- expand env e2
                    return $ Let x e1 e2

  h@(Hole t)  -> fromMaybe (throwError $ UnfilledHole h)
                   $ fmap (return . Con . typedName t) m

  Inst e1 n   -> do expand (ds, Just n) e1



-- * Reduction errors

data ReductionError n
  = UnfilledHole  (Expr n)
  | UnknownName n (Expr n)
  | RemainingHole (Expr n)
  | RemainingInst (Expr n)
  | MiscError String

instance IsName n => Show (ReductionError n) where
  show (UnfilledHole e)
    = printf "Unfilled hole in %s" (show (mapNames collapse e))
  show (UnknownName n e)
    = printf "Unknown name %s in %s" (collapse n) (show $ mapNames collapse e)
  show (RemainingHole e)
    = printf "Remaining holes in %s" (show $ mapNames collapse e)
  show (RemainingInst e)
    = printf "Remaining instantiation in %s" (show $ mapNames collapse e)
  show (MiscError str)
    = str

instance Error (ReductionError n) where
  strMsg = MiscError

addInfo :: Expr n -> ReductionError n -> Either (ReductionError n) a
addInfo e (RemainingHole _) = throwError $ RemainingHole e
addInfo _  err              = throwError $ err
