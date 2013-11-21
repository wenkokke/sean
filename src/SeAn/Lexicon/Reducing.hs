module SeAn.Lexicon.Reducing (reduce) where

import SeAn.Lexicon.Base
import SeAn.Lexicon.Typing

import Text.Printf (printf)

import Control.Arrow ((***))
import Control.Exception
import Control.Monad.Error
import Control.Monad.Supply

import Data.List (elemIndex,partition,(\\))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)


-- |Check if a declaration has unfilled holes.
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

reduce :: IsName n => Prog n -> Either (ReductionError n) (Prog n)
reduce p@(Prog ds) = do
  env <- foldM reduceDecl (toEnv as) es
  return $ Prog (M.elems env)
  where
    (as,es) = partition isAnnotation ds

reduceDecl :: IsName n => Env n -> Decl n -> Either (ReductionError n) (Env n)
reduceDecl env (Decl n e1) = do
  e2 <- reduceExpr env e1
  return $ M.insert (noType n) (Decl n e2) env

reduceExpr :: IsName n => Env n -> Expr n -> Either (ReductionError n) (Expr n)
reduceExpr env e
  = fmap (\e -> evalSupply e freshNames)
  $ fmap betaReduce
  $ fmap rewriteLet
  $ expandFill (env, Nothing) e `catchError` addInfo e

freshNames :: [Name]
freshNames = map (printf "?%d") ([0..] :: [Int])


-- * Substitution and beta conversion

betaReduce :: IsName n => Expr n -> Supply Name (Expr n)
betaReduce exp = case exp of
  v@(Var _)             -> do return v
  c@(Con _)             -> do return c
  a@(App (Abs n e1) e2) -> do a <- sub n e2 e1
                              betaReduce a
  a@(Abs n e1)          -> do e1 <- betaReduce e1
                              return (Abs n e1)
  a@(App e1 e2)         -> do e1 <- betaReduce e1
                              let a = App e1 e2
                              case e1 of
                                Abs _ _ -> betaReduce a
                                _       -> return a


sub :: IsName n => n -> Expr n -> Expr n -> Supply Name (Expr n)
sub x r v@(Var y)     = do return $ if x == y then r else v
sub x r c@(Con _)     = do return $ c
sub x r a@(Abs y e1)  = if x == y
                        then return a
                        else if y `elem` fv r
                             then do n <- supply
                                     let z = rename n y
                                     e1 <- sub y (Var z) e1
                                     e1 <- sub x r e1
                                     return $ Abs z e1
                             else do e1 <- sub x r e1
                                     return $ Abs y e1
sub x r a@(App e1 e2) = do e1 <- sub x r e1
                           e2 <- sub x r e2
                           return $ App e1 e2

-- |Compute the free variables in an expression.
fv :: Eq n => Expr n -> [n]
fv (Con _)       = [ ]
fv (Var n)       = [n]
fv (Abs n e1)    = fv e1 \\ [n]
fv (App   e1 e2) = fv e1 ++ fv e2



-- * Removal of let-bindings trough rewrites

-- |Removes let-bindings by rewriting them to lambda terms.
rewriteLet :: Expr n -> Expr n
rewriteLet exp = case exp of
  c@(Con _)   -> c
  v@(Var _)   -> v
  Abs n e1    -> Abs n (rewriteLet e1)
  App   e1 e2 -> App (rewriteLet e1) (rewriteLet e2)
  Let n e1 e2 -> App (Abs n e2) e1



-- * Expansion through lookups

type Env n = [((n,Type), Decl (n,Type))]

toEnv :: IsName n => [Decl (n,Type)] -> Env n
toEnv = map (\d@(Decl n _) -> (n,d))

findInEnv :: IsName n => (n,Type) -> Env n -> Maybe (Decl (n,Type))
findInEnv (n,ty) = undefined
  where
    unifyTypes :: ((n,Type),Decl (n,Type)) -> Supply Name (Maybe (Decl (n,Type)))
    unifyTypes ((_,t1),d@(Decl (_,t2) _)) = case unify t1 t2 of
      Left  _ -> return Nothing
      Right s -> undefined
    filterByName :: Env n -> Env n
    filterByName = filter ((==n) . fst . fst)

-- |Expand lambda terms by recursive lookup in an environment, and fill its holes.
expandFill :: IsName n => (Env n, Maybe Name) -> Expr n -> Either (ReductionError n) (Expr n)
expandFill env@(ds,m) exp = case exp of

  c@(Con n)       -> if isReservedName (base n)
                     then do return c
                     else do e1 <- findByName ds n
                             expandFill env e1

  v@(Var _)       -> do return v

  a@(Abs x e1)    -> do e1 <- expandFill env e1
                        return $ Abs x e1

  a@(App e1 e2)   -> do e1 <- expandFill env e1
                        e2 <- expandFill env e2
                        return $ App e1 e2

  l@(Let x e1 e2) -> do e1 <- expandFill env e1
                        e2 <- expandFill env e2
                        return $ Let x e1 e2

  h@(Hole t)      -> fromMaybe (throwError $ UnfilledHole h)
                     $ fmap (return . Con . retype t) m

  i@(Inst e1 n)   -> do expandFill (ds, Just n) e1


-- |Run unification returning a value in the Maybe monad.
unify' :: Type -> Type -> Supply TyName (Maybe TySubst)
unify' t1 t2 = errorToMaybe (unify t1 t2)


-- |Lookup an expression in an environment, possibly fail.
findByName :: IsName n => Env n -> n -> Either (ReductionError n) (Expr n)
findByName ds n = maybe ifNothing ifJust maybeExpr
  where
    ifJust    = return
    ifNothing = throwError (UnknownName n (Con n))
    maybeExpr = do (Decl _ e) <- M.lookup (noType n) ds; return e

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
