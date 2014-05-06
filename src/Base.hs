{-# LANGUAGE GADTs #-}

module Base where

import Prelude hiding (abs,fst,snd)
import Control.Applicative ((<|>))
import Data.Map (Map,(!))
import qualified Data.Map as M (lookup,fromList)
import qualified Data.List as L (union,intercalate)
import Text.Printf (PrintfArg,printf)

-- * Types and terms

type Name  = String
type Label = [Int]

type Env   = Map Name Expr
type TyEnv = Map Name Type
type TyAnn = Maybe Type

data Type where
  TyCon  :: Name -> Type
  TyVar  :: Name -> Type
  TyArr  :: Type -> Type -> Type
  TyPair :: Type -> Type -> Type
  deriving (Eq)

instance Show Type where
  show t
    | null vars = showNoVars t
    | otherwise = printf "∀%s.%s" (L.intercalate "," vars) (showNoVars t)
    where
      vars = freeTypeVars t
      showNoVars (TyCon n) = n
      showNoVars (TyVar n) = n
      showNoVars (TyArr t1 t2) = printf "%s → %s" (showParArr t1) (showNoVars t2)
      showNoVars (TyPair t1 t2) = printf "%s * %s" (showParAll t1) (showParAll t2)
      showParArr  t'@(TyArr {}) = printf "(%s)" (showNoVars t')
      showParArr  t' = showNoVars t'
      showParPair t'@(TyPair {}) = printf "(%s)" (showNoVars t')
      showParPair t' = showNoVars t'
      showParAll  t'@(TyArr {}) = showParArr t'
      showParAll  t'@(TyPair {}) = showParPair t'
      showParAll  t' = showNoVars t'

data Expr where
  Var    :: Name -> TyAnn -> Expr
  Abs    :: Name -> Expr -> TyAnn -> Expr
  App    :: Expr -> Expr -> TyAnn -> Expr
  Pair   :: Expr -> Expr -> TyAnn -> Expr
  Case   :: Name -> Name -> Expr -> TyAnn -> Expr
  Obj    :: Int -> TyAnn -> Expr
  Set    :: [Expr] -> TyAnn -> Expr
  Hole   :: TyAnn -> Expr
  Plug   :: Expr -> Expr -> TyAnn -> Expr
  deriving (Eq)

showTyAnn :: PrintfArg a => a -> TyAnn -> String
showTyAnn e Nothing  = printf "%s" e
showTyAnn e (Just t) = printf "%s:%s" e (show t)

instance Show Expr where
  show expr = case expr of
    (Var n t)      -> showTyAnn n t
    (Abs n e _)    -> printf "\\%s.%s" n (show e)
    (App f e _)    -> printf "%s %s" (show f) (showParens e)
    (Pair x y _)   -> printf "(%s, %s)" (show x) (show y)
    (Case x y e _) -> printf "\\(%s,%s).%s" x y (show e)
    (Obj x _)      -> printf "%d" x
    (Set xs _)     -> printf "{%s}" (L.intercalate "," $ map show xs)
    (Hole t)       -> printf "_:%s" (show t)
    (Plug e c _)   -> printf "%s[%s]" (showParens c) (show e)
    where
      showParens e@(App {}) = printf "(%s)" (show e)
      showParens e = show e


data Decl where
  Decl   :: Name -> TyAnn -> Expr -> Decl
  deriving (Eq)

instance Show Decl where
  show (Decl n Nothing e)  =
    printf "%s = %s" n (show e)
  show (Decl n (Just t) e) =
    printf "%s : %s\n%s" n (show t) (show (Decl n Nothing e))



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
pair e1 e2  = Pair e1 e2 Nothing
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
freeTypeVars (TyPair t1 t2) = freeTypeVars t1 `L.union` freeTypeVars t2



-- * Type annotations


class HasType e where
  annotate :: e -> TyAnn -> e

instance HasType Decl where
  annotate (Decl n t1 e) t2    = Decl n (t2 <|> t1) e

instance HasType Expr where
  annotate (Var n t1) t2        = Var n (t2 <|> t1)
  annotate (Abs n e t1) t2      = Abs n e (t2 <|> t1)
  annotate (App e1 e2 t1) t2    = App e1 e2 (t2 <|> t1)
  annotate (Pair e1 e2 t1) t2   = Pair e1 e2 (t2 <|> t1)
  annotate (Case n1 n2 e t1) t2 = Case n1 n2 e (t2 <|> t1)
  annotate (Obj i t1) t2        = Obj i (t2 <|> t1)
  annotate (Set es t1) t2       = Set es (t2 <|> t1)
  annotate (Hole t1) t2         = Hole (t2 <|> t1)
  annotate (Plug e c t1) t2     = Plug e c (t2 <|> t1)
