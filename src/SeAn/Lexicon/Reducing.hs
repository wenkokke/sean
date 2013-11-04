module SeAn.Lexicon.Reducing (reduceExpr,toDeBruijn,toEnv,global) where

import SeAn.Lexicon.Base hiding (Expr (..))
import qualified SeAn.Lexicon.Base as S

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (elemIndex)
import qualified Data.List as L

data Expr n
   = Con n
   | Var Int
   | Abs n (Expr n)
   | App   (Expr n) (Expr n)
   deriving (Eq,Show)

data Env n = Env
  { global :: n -> Maybe (S.Expr n)
  , local  :: [n]
  , inst   :: Maybe n
  }

reduceExpr :: IsName n => Prog n -> S.Expr n -> S.Expr n
reduceExpr p e = fromDeBruijn [] deBruijn
  where
    env = toEnv p
    deBruijn = toDeBruijn env e

-- |Builds an environment from a program.
toEnv :: IsName n => Prog n -> Env n
toEnv (Prog ds) = Env
  { global = let m = M.fromList (map (\(Decl n e) -> (n,e)) ds)
             in \n -> M.lookup n m
  , local  = []
  , inst   = Nothing
  }

-- |Convert an expression to a de Bruijn style lambda expression
--  and perform several rewrites in the meantime:
--
--    1. expand constant expressions using the environment
--    2. rewrites *let* expressions:
--       @let x = e1 in e2@ rewrites to @(\x.e2) e1@
--    3. rewrites holes and instantiation expressions
toDeBruijn :: IsName n => Env n -> S.Expr n -> Expr n
toDeBruijn env exp = case exp of
  S.Con n       -> if isReservedName (base n)
                    then Con n
                    else case global env n of
                     Just e  -> toDeBruijn env e
                     Nothing -> error ("reduce: unknown function " ++ collapse n)
  S.Var n       -> case n `elemIndex` local env of
                    Just ix -> Var ix
                    Nothing -> error ("reduce: unbound variable " ++ collapse n)
  S.Abs n e     -> Abs n (toDeBruijn (n `addlocal` env) e)
  S.App   e1 e2 -> App (toDeBruijn env e1) (toDeBruijn env e2)
  S.Let n e1 e2 -> App (Abs n (toDeBruijn (n `addlocal` env) e2)) (toDeBruijn env e1)
  S.Hole t      -> case inst env of
                    Just n  -> Con (settype n t)
                    Nothing -> error ("reduce: uninstantiated hole")
  S.Inst n a    -> case global env n of
                    Just e  -> toDeBruijn (n `addinst` env) e
                    Nothing -> error ("reduce: unknown function " ++ collapse n)

-- |Adds a variable name to the list of bound variables.
addlocal :: n -> Env n -> Env n
addlocal n env = env { local = n : local env }

-- |Adds an instantiation name as the filling expression.
addinst :: n -> Env n -> Env n
addinst n env = env { inst = Just n }

-- |Convert a de Bruijn style lambda expression back to a regular
--  lambda expression.
fromDeBruijn :: IsName n => [n] -> Expr n -> S.Expr n
fromDeBruijn ns exp = case exp of
  Con n     -> S.Con n
  Var i     -> S.Var (ns !! i)
  Abs n e   -> S.Abs n (fromDeBruijn (n:ns) e)
  App e1 e2 -> S.App (fromDeBruijn ns e1) (fromDeBruijn ns e2)
