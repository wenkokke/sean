{-# LANGUAGE GADTs, FlexibleInstances #-}

module Base where

import Prelude hiding (abs,fst,snd)
import Control.Monad (msum)
import Control.Applicative ((<$>),(<|>))
import Data.Map (Map,(!))
import qualified Data.Map as M (lookup,fromList)
import qualified Data.List as L (union,intercalate,elemIndex)
import Text.Printf (PrintfArg,printf)

-- * Types and terms

data Prog where
  Prog :: Int -> [Decl] -> [Rewr] -> Prog
  deriving (Eq)

instance Show Prog where
  show (Prog size ds rw) =
    unlines $ [printf "domain_size = %d" size] ++ (show <$> ds) ++ (show <$> rw)

type Name  = String
type Label = [Int]

type Env   = Map Name Expr
type TyEnv = Map Name Type
type RwEnv = Map Expr Expr
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
      showNoVars (TyPair t1 t2) = printf "%s * %s" (showParPair t1) (showNoVars t2)
      showParArr  t'@(TyArr {}) = printf "(%s)" (showNoVars t')
      showParArr  t' = showNoVars t'
      showParPair t'@(TyPair {}) = printf "(%s)" (showNoVars t')
      showParPair t' = showNoVars t'


data Expr where
  Var    :: Name -> TyAnn -> Expr
  Abs    :: Name -> Expr -> TyAnn -> Expr
  App    :: Expr -> Expr -> TyAnn -> Expr
  Obj    :: Int -> TyAnn -> Expr
  Rel1   :: [Expr] -> TyAnn -> Expr
  Rel2   :: [(Expr,Expr)] -> TyAnn -> Expr
  Rel3   :: [(Expr,Expr,Expr)] -> TyAnn -> Expr
  Hole   :: TyAnn -> Expr
  Plug   :: Expr -> Expr -> TyAnn -> Expr


-- equality up to alpha conversion
alphaEq :: [Name] -> Expr -> Expr -> Bool
alphaEq vs x y = case (x , y) of
  (Var x1 _        , Var x2 _)        ->
    case (L.elemIndex x1 vs , L.elemIndex x2 vs) of
      (Just i1 , Just i2) -> i1 == i2
      (_       , _      ) -> False
  (Abs x1 e1 _     , Abs x2 e2 _)     ->
    x1 == x2 && alphaEq (x1 : vs) e1 e2
  (App f1 x1 _     , App f2 x2 _)     ->
    alphaEq vs f1 f2 && alphaEq vs x1 x2
  (Obj i1 _        , Obj i2 _)        ->
    i1 == i2
  (Rel1 xs1 _      , Rel1 xs2 _)       ->
    length xs1 == length xs2 && and (zipWith (alphaEq vs) xs1 xs2)
  (Rel2 xs1 _      , Rel2 xs2 _)       ->
    length xs1 == length xs2 && all (\((x1,y1),(x2,y2)) -> alphaEq vs x1 x2 && alphaEq vs y1 y2) (zip xs1 xs2)
  (Rel3 xs1 _      , Rel3 xs2 _)       ->
    length xs1 == length xs2 && all (\((x1,y1,z1),(x2,y2,z2)) -> alphaEq vs x1 x2 && alphaEq vs y1 y2 && alphaEq vs z1 z2) (zip xs1 xs2)
  (Hole _          , Hole _)          -> True
  (Plug e1 c1 _    , Plug e2 c2 _)    ->
    alphaEq vs e1 e2 && alphaEq vs c1 c2
  (_               , _)               -> False


instance Eq Expr where
  (==) = alphaEq []


showTyAnn :: PrintfArg a => a -> TyAnn -> String
showTyAnn e Nothing  = printf "%s" e
showTyAnn e (Just t) = printf "%s:%s" e (show t)


instance Show Expr where
  show expr = case expr of
    (Var n t)      -> showTyAnn n t
    (Abs n e _)    -> printf "\\%s.%s" n (show e)
    (App f e _)    -> printf "%s %s" (show f) (showParens e)
    (Obj x _)      -> printf "%d" x
    (Rel1 xs _)    -> printf "{%s}" (L.intercalate "," $ map show xs)
    (Rel2 xs _)    -> printf "{%s}" (L.intercalate "," $ map show xs)
    (Rel3 xs _)    -> printf "{%s}" (L.intercalate "," $ map show xs)
    (Hole t)       -> printf "_:%s" (show t)
    (Plug e c _)   -> printf "%s[%s]" (showParens c) (show e)
    where
      showParens e@(App {}) = printf "(%s)" (show e)
      showParens e = show e


data Rewr where
  Rewr :: Expr -> Expr -> Rewr
  deriving (Eq)

instance Show Rewr where
  show (Rewr e1 e2) =
    printf "%s -> %s" (show e1) (show e2)

data Decl where
  Decl :: Name -> TyAnn -> Expr -> Decl
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
obj i       = Obj i (Just $ TyCon "e")
rel1 es     = Rel1 es Nothing
rel2 es     = Rel2 es Nothing
rel3 es     = Rel3 es Nothing
ggq gq xs e = foldr ((app (prim gq) . ) . abs) e xs
univ        = ggq "FORALL"
exis        = ggq "EXISTS"
iota        = ggq "IOTA"


-- * Free variables

freeTypeVars :: Type -> [Name]
freeTypeVars (TyCon _) = []
freeTypeVars (TyVar n) = [n]
freeTypeVars (TyArr t1 t2) = freeTypeVars t1 `L.union` freeTypeVars t2
freeTypeVars (TyPair t1 t2) = freeTypeVars t1 `L.union` freeTypeVars t2


-- * Hole Types

getHoleType :: Expr -> Maybe Type
getHoleType (Var _ _)     = Nothing
getHoleType (Abs _ e _)   = getHoleType e
getHoleType (App e1 e2 _) = getHoleType e1 <|> getHoleType e2
getHoleType (Obj _ _)     = Nothing
getHoleType (Rel1 es _)   = msum (map getHoleType es)
getHoleType (Rel2 es _)   = msum (map (\(e1,e2) -> getHoleType e1 <|> getHoleType e2) es)
getHoleType (Rel3 es _)   = msum (map (\(e1,e2,e3) -> getHoleType e1 <|> getHoleType e2 <|> getHoleType e3) es)
getHoleType (Hole t1)     = t1
getHoleType (Plug e c _)  = getHoleType c <|> getHoleType e



-- * Type annotations


class HasType e where
  annotate :: e -> TyAnn -> e

instance HasType Decl where
  annotate (Decl n t1 e) t2 = Decl n (t2 <|> t1) e

instance HasType Expr where
  annotate (Var n t1) t2     = Var n (t2 <|> t1)
  annotate (Abs n e t1) t2   = Abs n e (t2 <|> t1)
  annotate (App e1 e2 t1) t2 = App e1 e2 (t2 <|> t1)
  annotate (Obj i t1) t2     = Obj i (t2 <|> t1)
  annotate (Rel1 es t1) t2   = Rel1 es (t2 <|> t1)
  annotate (Rel2 es t1) t2   = Rel2 es (t2 <|> t1)
  annotate (Rel3 es t1) t2   = Rel3 es (t2 <|> t1)
  annotate (Hole t1) t2      = Hole (t2 <|> t1)
  annotate (Plug e c t1) t2  = Plug e c (t2 <|> t1)

instance HasType (Expr,Expr) where
  annotate (e1 , e2) (Just (TyPair t1 t2)) =
    (annotate e1 (Just t1) , annotate e2 (Just t2))
  annotate e _ = e

instance HasType (Expr,Expr,Expr) where
  annotate (e1 , e2 , e3) (Just (TyPair t1 (TyPair t2 t3))) =
    (annotate e1 (Just t1) , annotate e2 (Just t2) , annotate e3 (Just t3))
  annotate e _ = e
