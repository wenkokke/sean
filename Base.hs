{-# LANGUAGE GADTs #-}

module Base where

import Prelude hiding (abs)
import Control.Applicative ((<|>))
import Data.Map (Map,(!))
import qualified Data.Map as M (lookup,fromList)
import qualified Data.List as L (union)
import qualified Data.String.Utils as S (join)
import Text.Printf (printf)

-- * Types and terms

type Name  = String
type Label = [Int]

type Env   = Map Name Expr
type TyEnv = Map Name Type
type TyAnn = Maybe Type

data Type where
  TyCon :: Name -> Type
  TyVar :: Name -> Type
  TyArr :: Type -> Type -> Type
  deriving (Eq)

instance Show Type where
  show t
    | null vars = showNoVars t
    | otherwise = printf "∀%s.%s" (S.join "," vars) (showNoVars t)
    where
      vars = freeTypeVars t
      showNoVars (TyCon n) = n
      showNoVars (TyVar n) = n
      showNoVars (TyArr t1 t2) = printf "%s → %s" (showParens t1) (showNoVars t2)
      showParens t@(TyArr _ _) = printf "(%s)" (showNoVars t)
      showParens t = showNoVars t

data Expr where
  Var    :: Name -> TyAnn -> Expr
  Abs    :: Name -> Expr -> TyAnn -> Expr
  App    :: Expr -> Expr -> TyAnn -> Expr
  Obj    :: Int -> TyAnn -> Expr
  Set    :: [Expr] -> TyAnn -> Expr
  Hole   :: TyAnn -> Expr
  Plug   :: Expr -> Expr -> TyAnn -> Expr
  deriving (Eq,Show)

data Decl where
  Decl   :: Name -> TyAnn -> Expr -> Decl
  deriving (Eq,Show)



-- * Primitive types

-- *** Standard Type Environment

stdTyEnv :: TyEnv
stdTyEnv = M.fromList
  [ ("TRUE"    , t)
  , ("FALSE"   , t)
  , ("NOT"     , t ~> t)
  , ("AND"     , t ~> (t ~> t))
  , ("OR"      , t ~> (t ~> t))
  , ("IMPLIES" , t ~> (t ~> t))
  , ("EQUIV"   , t ~> (t ~> t))
  , ("EQUALS"  , e ~> (e ~> t))
  , ("IOTA"    , (e ~> t) ~> e)
  , ("FORALL"  , (e ~> t) ~> t)
  , ("EXISTS"  , (e ~> t) ~> t)
  ]
  where
    (e,t,(~>)) = (TyCon "e",TyCon "t",TyArr)





-- * Smart constructors

prim :: String -> Expr
prim n = Var n (M.lookup n stdTyEnv)

fun1 :: String -> Expr -> Expr
fun1 n e = App (prim n) e (Just b)
  where
    (TyArr _ b) = stdTyEnv ! n

fun2 :: String -> Expr -> Expr -> Expr
fun2 n e1 e2 = App (App (prim n) e1 (Just b2c)) e2 (Just c)
  where
    (TyArr _ b2c@(TyArr _ c)) = stdTyEnv ! n

var n       = Var n Nothing
abs n e     = Abs n e Nothing
absn xs e   = foldr abs e xs
app e1 e2   = App e1 e2 Nothing
obj i       = Obj i (Just $ TyCon "e")
set es      = Set es Nothing
ggq gq xs e = foldr ((app (prim gq) . ) . abs) e xs
univ        = ggq "FORALL"
exis        = ggq "EXISTS"
iota        = ggq "IOTA"



-- * Binary operator symbols

bins :: [[(String , Expr -> Expr -> Expr)]]
bins =
  [ [ ("=="  , fun2 "EQUALS") ]
  , [ ("\\/" , fun2 "OR") , ("/\\" , fun2 "AND") ]
  , [ ("=>"  , fun2 "IMPLIES") , ("<=" , flip (fun2 "IMPLIES")) ]
  , [ ("<=>" , fun2 "EQUIV") ]
  ]


-- * Free variables

freeTypeVars :: Type -> [Name]
freeTypeVars (TyCon _) = []
freeTypeVars (TyVar n) = [n]
freeTypeVars (TyArr t1 t2) = freeTypeVars t1 `L.union` freeTypeVars t2



-- * Type annotations


class HasType e where
  annotate :: e -> TyAnn -> e

instance HasType Decl where
  annotate (Decl n t1 e) t2    = Decl n (t2 <|> t1) e

instance HasType Expr where
  annotate (Var n t1) t2       = Var n (t2 <|> t1)
  annotate (Abs n e t1) t2     = Abs n e (t2 <|> t1)
  annotate (App e1 e2 t1) t2   = App e1 e2 (t2 <|> t1)
  annotate (Plug e c t1) t2    = Plug e c (t2 <|> t1)
  annotate (Obj i t1) t2       = Obj i (t2 <|> t1)
  annotate (Set es t1) t2      = Set es (t2 <|> t1)
  annotate (Hole t1) t2        = Hole (t2 <|> t1)
