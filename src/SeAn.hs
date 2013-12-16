{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}

module SeAn.Base where

import Prelude hiding (True,False,abs,and,or)
import Control.Arrow (second)
import Control.Applicative ((<$),(<$>),(<|>),(<*>))
import Control.Monad (when,foldM)
import Control.Monad.Error (ErrorT,runErrorT,mapErrorT,lift)
import Control.Monad.Supply
import qualified Data.Bool as B
import Data.Char (isDigit,digitToInt)
import Data.Either (lefts)
import qualified Data.List as L
import Data.Map (Map,(!))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.ParserCombinators.UU ((<?>),(<??>),pList,pList1,pList1Sep,pMaybe,pChainl)
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils
import Text.Printf (printf,PrintfArg)

type Name = String
type Label = [Int]

type Env   = Map Name Expr
type CtEnv = Map Name Context
type TyEnv = Map Name Type
type Size  = Int
type Error a = ErrorT String (Supply Name) a

run :: Size -> [String] -> Either String (Env , CtEnv , TyEnv)
run size ds0 = supplyFreshNames ds3
  where
    ds1 :: [Decl]
    ds1 = map (runParser "stdin" pDecl) ds0
    ds2 :: Error [Decl]
    ds2 = disambiguate ds1
    ds3 :: Error (Env , CtEnv , TyEnv)
    ds3 = ds2 >>= eval size

isError :: Error a -> Bool
isError = either (const B.True) (const B.False) . supplyFreshNames



-- * Abstract Syntax Trees

data Type where
  TyCon :: Name -> Type
  TyVar :: Name -> Type
  TyArr :: Type -> Type -> Type
  deriving (Eq,Show)

type TyAnn = Maybe Type

data Decl where
  Expr :: Name -> TyAnn -> Expr    -> Decl
  Context :: Name -> TyAnn -> Context -> Decl
  deriving (Eq,Show)

data Expr where
  Var    :: Name -> TyAnn -> Expr
  Abs    :: Name -> Expr -> TyAnn -> Expr
  App    :: Expr -> Expr -> TyAnn -> Expr
  Obj    :: Int -> TyAnn -> Expr
  Set    :: [Expr] -> TyAnn -> Expr
  Plug   :: Expr -> Context -> TyAnn -> Expr
  deriving (Eq,Show)

data Context where
  CVar :: Name -> TyAnn -> Context
  AppL :: Context -> Expr -> TyAnn -> Context
  AppR :: Expr -> Context -> TyAnn -> Context
  Hole :: TyAnn -> Context
  deriving (Eq,Show)


-- * Global constants

holeName :: Name
holeName = "[]"

labelSep :: Char
labelSep = '#'

-- * Parsing

-- ** Parsing types and type annotations

pType :: Parser Type
pType = foldr1 TyArr <$> pList1 pAtom <?> "type"
  where
    pAtom = TyCon . return <$> pAnySym "et"
        <|> pParens pType

pTyAnn :: Parser Type
pTyAnn = iI ':' pType Ii <?> "type annotation"

pOptTyAnn :: HasTyAnn a => Parser (a -> a)
pOptTyAnn = flip setTyAnn . Just <$> pTyAnn

class HasTyAnn e where
  setTyAnn :: e -> TyAnn -> e

instance HasTyAnn Expr where
  setTyAnn (Var n t1)     t2 = Var n (t2 <|> t1)
  setTyAnn (Abs n e t1)   t2 = Abs n e (t2 <|> t1)
  setTyAnn (App e1 e2 t1) t2 = App e1 e2 (t2 <|> t1)
  setTyAnn (Plug e c t1)  t2 = Plug e c (t2 <|> t1)
  setTyAnn (Obj i t1)     t2 = Obj i (t2 <|> t1)
  setTyAnn (Set es t1)    t2 = Set es (t2 <|> t1)

instance HasTyAnn Context where
  setTyAnn (CVar n t1)   t2 = CVar n (t2 <|> t1)
  setTyAnn (AppL c e t1) t2 = AppL c e (t2 <|> t1)
  setTyAnn (AppR e c t1) t2 = AppR e c (t2 <|> t1)
  setTyAnn (Hole t1)     t2 = Hole (t2 <|> t1)

-- ** Parsing names

pName  :: Parser Name
pName  = lexeme (iI (:) pLetter (pList pAlphaNum_) Ii) <?> "name"

pNames :: Parser [Name]
pNames = pList1Sep pSpaces pName

pAlphaNum_ :: Parser Char
pAlphaNum_ = pLetter <|> pDigit <|> pSym '_'

-- ** Parsing expressions

pExpr :: Parser Expr
pExpr = pChainl (app <$ pSpaces) pExprAtom

pVar :: Parser Expr
pVar = iI Var pName (pMaybe pTyAnn) Ii

pObj :: Parser Expr
pObj = iI Obj pNatural (pMaybe pTyAnn) Ii

pSet :: Parser Expr
pSet = Set <$> pBraces (pList1Sep pComma pExpr) <*> pMaybe pTyAnn

pGGQ :: Parser Expr
pGGQ  = pAbs <|> pUniv <|> pExis <|> pIota
  where
    pAbs  = iI absn '\\' pNames '.' pExpr Ii <?> "lambda abstraction"
    pUniv = iI univ '!'  pNames '.' pExpr Ii <?> "universal quantifier"
    pExis = iI exis '?'  pNames '.' pExpr Ii <?> "existential quantifier"
    pIota = iI iota 'i'  pNames '.' pExpr Ii <?> "iota"

pExprAtom :: Parser Expr
pExprAtom  = pVar <|> pGGQ <|> pObj <|> pSet <|> (pParens pExpr <??> pOptTyAnn)

-- ** Parsing contexts

pContext :: Parser Context
pContext = pAppL <|> pAppR <|> pHole
  where
    pAppL  = appl <$> pContextAtom <*> pExprAtom
    pAppR  = appr <$> pExprAtom <*> pContextAtom

pHole :: Parser Context
pHole = iI Hole holeName (pMaybe pTyAnn) Ii

pContextAtom :: Parser Context
pContextAtom = pHole <|> (pParens pContext <??> pOptTyAnn)

-- ** Parsing declarations

pDecl :: Parser Decl
pDecl = iI Expr pName (pMaybe pTyAnn) '=' pExpr Ii
    <|> iI Context '@' pName (pMaybe pTyAnn) '=' pContext Ii

-- ** Smart constructors for expressions and contexts

var n       = Var n Nothing
abs n e     = Abs n e Nothing
absn xs e   = foldr abs e xs
app e1 e2   = App e1 e2 Nothing
obj i       = Obj i (Just $ TyCon "e")
set es      = Set es Nothing
appl e1 e2  = AppL e1 e2 Nothing
appr e1 e2  = AppR e1 e2 Nothing
ggq gq xs e = foldr ((app (Var gq (M.lookup gq stdTyEnv)) .) . abs) e xs
univ        = ggq "FORALL"
exis        = ggq "EXISTS"
iota        = ggq "IOTA"



-- * Disambiguation and Plugging

-- resolve:
--   split ambiguous name into unambiguous labelled names
--   use type annotations to rule out possible disambiguations
--
-- can we get the guarantee that sub-expressions are already
-- unambiguous? if we could, we could perform type inference
-- on those to rule out all other disambiguations.
--
-- let's see. we disambiguate top-down, as we do with type
-- inference. the problem is that disambiguating an expression
-- may lead to other expressions becoming ambiguous... BUT since
-- we can only use expressions after we define them, the expressions
-- that become ambiguous have to follow the current expression.
-- so, yes. we can get the invariant that sub-expressions are
-- already unambiguous.
--
-- so what we're going to do is the following: we're going to
-- hm. we should group the declarations that are ambiguous, i.e.
-- of which the type annotations are compatible. but let's put down
-- some ground rules here:
--
-- if we have two compatible definitions right after one another, then
-- they are only considered ambiguous starting the second definition.
-- this means that if we have any number of definitions between the
-- two compatible definitions, they are not in fact considered to be
-- ambiguous, so:
--
--     x = ...
--     y = ...[x]...
--     x = ...
--     z = ...[x]...
--
-- in the following program, the definition of y is not considered to
-- be ambiguous, since at that point there is only one definition of
-- x. however, z is ambiguous, since at that point we have two definitions
-- of x to choose from.
--
-- if we encounter an unambiguous definitions, we simple see that it
-- type checks and then store it in the environment for later use.
-- however, if we encounter an ambiguous definition, we:
--   * resolve it into a list of unambiguous definitions;
--   * check which of these definitions type check;
--     if none of them check out, we throw an exception
--     but if one or more of them check out we keep only those that
--     are valid formulae.
-- so what do we need?
--
-- we need to have an environment, which is nothing more that a list of
-- declarations that are guaranteed to be typed and unambiguous.
-- we need a list of which name/tyann pairs are known to be ambiguous
-- as in the amb function.
-- we need a list of which name/tyann pairs we have already seen.
--
-- a formula is ambiguous in if any of the following holds:
--   * its name/tyann pair is compatible with one we've already seen;
--   * its expression contains name/tyann pairs that are unbound and
--     that are already known to be ambiguous.

ident :: Decl -> (Name , TyAnn)
ident (Expr n t _) = (n , t)
ident (Context n t _) = (n , t)

class FreeIdents e where
  freeIdents :: e -> [(Name , TyAnn)]

instance FreeIdents Decl where
  freeIdents (Expr _ _ e) = freeIdents e
  freeIdents (Context _ _ c) = freeIdents c

instance FreeIdents Expr where
  freeIdents (Var n t)      = [(n , t)]
  freeIdents (Abs n1 e t1)  = L.filter (not . compatible (n1,t1)) (freeIdents e)
  freeIdents (App e1 e2 _)  = freeIdents e1 ++ freeIdents e2
  freeIdents (Plug e c _)   = freeIdents e  ++ freeIdents c
  freeIdents (Set es _)     = concatMap freeIdents es
  freeIdents (Obj _ _)      = []

instance FreeIdents Context where
  freeIdents (AppL e1 e2 _) = freeIdents e1 ++ freeIdents e2
  freeIdents (AppR e1 e2 _) = freeIdents e1 ++ freeIdents e2
  freeIdents (Hole _) = []

stdTyEnv :: TyEnv
stdTyEnv = M.fromList
  [ ("TRUE"    , t)
  , ("FALSE"   , t)
  , ("NOT"     , t ~> t)
  , ("AND"     , t ~> (t ~> t))
  , ("OR"      , t ~> (t ~> t))
  , ("IMPLIES" , t ~> (t ~> t))
  , ("SEILPMI" , t ~> (t ~> t))
  , ("EQUIV"   , t ~> (t ~> t))
  , ("EQUALS"  , e ~> (e ~> t))
  , ("IOTA"    , (e ~> t) ~> e)
  , ("FORALL"  , (e ~> t) ~> t)
  , ("EXISTS"  , (e ~> t) ~> t)
  ]
  where
    (e,t,(~>)) = (TyCon "e",TyCon "t",TyArr)

-- |Count number of equally named elements in the seen list.
count :: Name -> [(Name,TyAnn)] -> Int
count m = sum . map (\(n,_) -> if m == n then 1 else 0)

indexed :: Name -> [(Name,TyAnn)] -> [(Name,TyAnn)]
indexed = indexed' []
  where
    indexed' _ _ [] = []
    indexed' seen m ((n,t):ns)
      | m == n     = (relabel max n , t) : rest
      | otherwise = rest
      where
        max   = count n seen
        seen' = seen ++ [(n,t)]
        rest  = indexed' seen' m ns

class Resolvable e where
  resolve :: [(Name,TyAnn)] -> e -> [e]

instance Resolvable Expr where

  -- resolution rules for lambda terms
  resolve seen (App e1 e2 t) =
    [ App e1' e2' t | e1' <- resolve seen e1 , e2' <- resolve seen e2 ]
  resolve seen (Abs n e t) =
    [ Abs n e' t | e' <- resolve bind e ]
    where
      bind = filter ((/=n) . fst) seen
  resolve seen (Var n1 t1) =
    [ Var n2 (t1 >< t2) | (n2,t2) <- resl ]
    where
      resl = if null comp then [(n1 , t1)] else comp
      comp = filter (compatible (n1 , t1)) poss
      poss = indexed n1 seen

  -- resolution rules for objects and sets
  resolve _ o@(Obj _ _)   = return o
  resolve seen (Set es t) = Set <$> mapM (resolve seen) es <*> return t

instance Resolvable Context where
  resolve seen (AppL c e t) =
    [ AppL c' e' t | c' <- resolve seen c, e' <- resolve seen e ]
  resolve seen (AppR e c t) =
    [ AppR e' c' t | e' <- resolve seen e, c' <- resolve seen c ]
  resolve _ (Hole t) =
    return (Hole t)

instance Resolvable Decl where
  resolve seen (Expr n t e) =
    [ Expr    n t e' | e' <- resolve seen e ]
  resolve seen (Context n t c) =
    [ Context n t c' | c' <- resolve seen c ]

noPossibleResolution :: [Error a] -> Error ()
noPossibleResolution xs = fail msg
  where
    msg
      | length msgs == 1 = head msgs
      | otherwise       = "No possible resolution for ambiguous expression:\n"
                          ++ unlines msgs
    msgs :: [String]
    msgs = lefts (supplyFreshNames <$> xs)

disambiguate :: [Decl] -> Error [Decl]
disambiguate = disambiguate' [] [] [] stdTyEnv
  where
    disambiguate'
      :: [(Name,TyAnn)] -> [(Name,TyAnn)] -> [Decl] -> TyEnv -> [Decl] -> Error [Decl]
    disambiguate' _ _ acc _ [] = return (reverse acc)
    disambiguate' seen amb acc env (d:ds)
      | cond1 || cond2 = do

        let i0       = count n seen
        let resolved = resolve seen d
        let typed    = map (infer stdTyEnv) resolved

        when (all isError typed) (noPossibleResolution typed)
        wf <- map fst3 <$> sequence (filter (not . isError) typed)

        let wf'      = zipWith (rename . relabel) [i0 ..] wf
        let seen'    = seen ++ (ident <$> wf)
        let amb'     = ident d : amb
        let acc'     = wf' ++ acc
        let env'     = foldr (uncurry M.insert . second fromJust) env (ident <$> wf')

        disambiguate' seen' amb' acc' env' ds

      | otherwise = do
        (d' , t , _) <- infer env d

        let seen' = seen ++ [ident d']
        let acc'  = d' : acc
        let env'  = M.insert n t env

        disambiguate' seen' amb acc' env' ds

      where
        i = ident d
        n = fst i
        used  = freeIdents d
        cond1 = L.or [ n == m | (m,_) <- seen ]
        cond2 = L.or [ n == m | (n,_) <- used, (m,_) <- amb ]

        fst3 :: (a,b,c) -> a
        fst3 (x,_,_) = x

rename :: (Name -> Name) -> Decl -> Decl
rename f (Expr n t e) = Expr (f n) t e
rename f (Context n t c) = Context (f n) t c

relabel :: Int -> Name -> Name
relabel i = join . second (++ [i]) . split
  where
    split :: Name -> (Name,Label)
    split = second (map digitToInt . filter isDigit) . break (==labelSep)
    join :: (Name,Label) -> Name
    join = (\(n,ls) -> n ++ labelSep : ls) . second (concatMap show)



-- * Substitutions

class Apply f a where
  apply :: f -> a -> a

instance (Apply f a) => Apply [f] a where
  apply [] = id
  apply (f : fs) = apply f . apply fs

data TySubst where
  TySubst :: Name -> Type -> TySubst

instance Apply TySubst Type where
  apply (TySubst n1 t) = subst
    where
      subst v@(TyVar n2) = if n1 == n2 then t else v
      subst (TyArr t1 t2) = TyArr (subst t1) (subst t2)
      subst c = c

instance Apply TySubst TyAnn where
  apply s = fmap (apply s)

instance Apply TySubst Expr where
  apply s (Var n t)     = Var n (apply s t)
  apply s (Abs n e t)   = Abs n (apply s e) (apply s t)
  apply s (App e1 e2 t) = App (apply s e1) (apply s e2) (apply s t)
  apply s (Obj i t)     = Obj i (apply s t)
  apply s (Set es t)    = Set (map (apply s) es) (apply s t)
  apply s (Plug c e t)  = Plug (apply s c) (apply s e) (apply s t)

instance Apply TySubst Context where
  apply s (CVar n t)   = CVar n (apply s t)
  apply s (AppL c e t) = AppL (apply s c) (apply s e) (apply s t)
  apply s (AppR e c t) = AppR (apply s e) (apply s c) (apply s t)
  apply s (Hole t)     = Hole (apply s t)

instance Apply TySubst (Map Name Type) where
  apply s = M.map (apply s)

data Subst where
  Subst :: Name -> Expr -> Subst

-- TODO Substitution is not type-safe.
instance Apply Subst Expr where
  apply s@(Subst n1 r) = subst
    where
      subst v@(Var n2 _)   = if n1 == n2 then r else v
      subst a@(Abs n2 e t) = if n1 == n2 then a else Abs n2 (subst e) t
      subst (App e1 e2 t)  = App (subst e1) (subst e2) t
      subst o@(Obj _ _)    = o
      subst (Set es t)     = Set (map subst es) t
      subst (Plug e c t)   = Plug (subst e) (apply s c) t

instance Apply Subst Context where
  apply s@(Subst _ _) = subst
    where
      subst c@(CVar _ _) = c
      subst (AppL c e t) = AppL (subst c) (apply s e) t
      subst (AppR e c t) = AppR (apply s e) (subst c) t
      subst h@(Hole _)   = h



-- * Type Inference

supplyFreshNames :: Error a -> Either String a
supplyFreshNames e = evalSupply (runErrorT e) freshNames

freshNames :: [Name]
freshNames = letters ++ numbers
  where
  letters    = fmap (: []) (['A'..'D'] ++ ['F'..'S'] ++ ['U'..'Z'])
  numbers    = fmap (('T' :) . show) ([0 ..] :: [Int])

class Infer e where
  infer :: TyEnv -> e -> Error (e , Type , [TySubst])

instance Infer Decl where
  infer env (Expr n tyAnn e) = do
    (e' , t, s0) <- infer env e
    (r , s1) <- checkTyAnn tyAnn t
    return (Expr n (Just r) e' , r , s1 ++ s0)
  infer env (Context n tyAnn c) = do
    (c' , t, s0) <- infer env c
    (r , s1) <- checkTyAnn tyAnn t
    return (Context n (Just r) c' , r , s1 ++ s0)

instance Infer Expr where
  infer env (Var n tyAnn)    = inferVar env n tyAnn Var
  infer env (Abs n x tyAnn)  = inferAbs env n x tyAnn Abs
  infer env (App f x tyAnn)  = inferApp env f x tyAnn App
  infer env (Plug e c tyAnn) = inferPlug env e c tyAnn Plug
  infer env (Obj i tyAnn)    = inferObj env i tyAnn Obj
  infer env (Set es tyAnn)   = inferSet env es tyAnn Set

instance Infer Context where
  infer env (AppL c e tyAnn) = inferApp env c e tyAnn AppL
  infer env (AppR e c tyAnn) = inferApp env e c tyAnn AppR
  infer  _  (Hole (Just t))  = return (Hole (Just t) , t , [])
  infer  _  (Hole Nothing)   = fail "Untyped holes are unsupported"

find :: Name -> Map Name v -> Error v
find n env = maybe (fail (printf "Unbound identifier %s" n)) return (M.lookup n env)

-- TODO Allow variable case to bind a fresh type variable in the case of
--      unbound identifiers, and propagate these upwards. This is usefull
--      to allow expressions such as "ix.dog:et x" without first having to
--      define "dog", and to allow untyped holes.
inferVar env n tyAnn var = do
  a <- find n env
  (r , s) <- checkTyAnn tyAnn a
  return (var n (Just r) , r , s)

inferAbs env n x tyAnn abs = do
    a <- freshTyVar
    (x' , b , s0) <- infer (M.insert n a env) x
    (r , s1) <- checkTyAnn tyAnn (apply s0 a `TyArr` b)
    return (abs n x' (Just r) , r , s1 ++ s0)

inferApp env f x tyAnn app = do
    (f' , a2b , s0) <- infer env f
    (x' , a   , s1) <- infer (apply s0 env) x
    b <- freshTyVar
    s2 <- unifyIn (apply s1 a2b) (a `TyArr` b) (show $ app f' x' tyAnn)
    let f''  = apply s2 f'
    let x''  = apply s2 x'
    let b'   = apply s2 b
    (r , s3) <- checkTyAnn tyAnn b'
    return (app f'' x'' (Just r) , r , s3 ++ s2 ++ s1 ++ s0)

inferPlug env e c tyAnn plug = do
  (e' , t1 , s0) <- infer env e
  (c' , t2 , s1) <- infer (apply s0 env) c
  s2 <- unifyIn t1 t2 (show $ plug e c tyAnn)
  let t   = apply s2 t2
  let e'' = apply s2 e'
  let c'' = apply s2 c'
  (r , s3) <- checkTyAnn tyAnn t
  return (plug e'' c'' (Just r) , r , s3 ++ s2 ++ s1 ++ s0)

inferObj env i tyAnn obj = do
  let t = TyCon "e"
  (t' , s0) <- checkTyAnn tyAnn t
  return (obj i (Just t') , t' , s0)

inferSet env es tyAnn set = do
  (es' , t : ts , ss) <- split3 <$> mapM (infer env) es
  let e = show (set es tyAnn)
  (t' , s0) <- foldM (\a b -> unifyIn' a b e) (t , []) ts
  let es'' = map (\e -> setTyAnn e (Just t')) es'
  let t'' = TyArr t' (TyCon "t")
  (t''' , s1) <- checkTyAnn tyAnn t''
  return (set es'' (Just t''') , t''' , s1 ++ s0 ++ concat ss)
  where
    split3 :: [(a,b,c)] -> ([a],[b],[c])
    split3 [] = ([] , [] , [])
    split3 ((a,b,c) : rs) = case split3 rs of
      (as , bs , cs) -> (a : as , b : bs , c : cs)

    unifyIn' :: PrintfArg a => (Type,[TySubst]) -> Type -> a -> Error (Type,[TySubst])
    unifyIn' (a,s0) b e = do
      let b' = apply s0 b
      s1 <- unifyIn a b' e
      let a' = apply s1 a
      return (a' , s1 ++ s0)

checkTyAnn :: TyAnn -> Type -> Error (Type , [TySubst])
checkTyAnn  Nothing  t2 = return (t2 , [])
checkTyAnn (Just t1) t2 = do s <- unify t1 t2; return (apply s t2 , s)

compatible :: (Name , TyAnn) -> (Name , TyAnn) -> Bool
compatible (n1 , t1) (n2 , t2) = n1 == n2 && compTyAnn t1 t2
  where
    compTyAnn :: TyAnn -> TyAnn -> Bool
    compTyAnn  Nothing    _         = B.True
    compTyAnn  _          Nothing   = B.True
    compTyAnn (Just t1') (Just t2') = unifiable t1' t2'

(><) :: TyAnn -> TyAnn -> TyAnn
Nothing >< tyAnn = tyAnn
tyAnn >< Nothing = tyAnn
(Just t1)  >< (Just t2) = noerror
  where
    noerror = either (const Nothing) Just (supplyFreshNames unified)
    unified = do s <- unify t1 t2; return (apply s t1)


-- * Unification

unifyIn :: PrintfArg a => Type -> Type -> a -> Error [TySubst]
unifyIn t1 t2 c = mapError (\e -> printf "%s in %s" e c) (unify t1 t2)

mapError :: (String -> String) -> Error a -> Error a
mapError f = mapErrorT (\s -> mapLeft f <$> s)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

unify :: Type -> Type -> Error [TySubst]
unify (TyCon n1) (TyCon n2) =
  if n1 /= n2 then cannotUnify n1 n2 else return []
unify (TyArr a1 b1) (TyArr a2 b2) =
  do s1 <- unify a1 a2
     s2 <- unify (apply s1 b1) (apply s1 b2)
     return (s2 ++ s1)
unify t (TyVar n) = occursCheck n t
unify (TyVar n) t = occursCheck n t
unify t1 t2 = cannotUnify (show t1) (show t2)

unifiable :: Type -> Type -> Bool
unifiable t1 t2 = not (isError (unify t1 t2))

cannotUnify :: (PrintfArg t1, PrintfArg t2) => t1 -> t2 -> Error a
cannotUnify t1 t2 = fail (printf "Cannot unify %s and %s" t1 t2)

occursCheck :: Name -> Type -> Error [TySubst]
occursCheck n t
  | n `occurs` t = fail (printf "%s occurs in %s" n (show t))
  | otherwise    = return [TySubst n t]

occurs :: Name -> Type -> Bool
occurs n t = n `elem` freeTypeVars t

freshTyVar :: Error Type
freshTyVar = return . TyVar =<< lift supply

freeTypeVars :: Type -> [Name]
freeTypeVars (TyCon _) = []
freeTypeVars (TyVar n) = [n]
freeTypeVars (TyArr t1 t2) = freeTypeVars t1 `L.union` freeTypeVars t2



-- * Evaluation Strategies

plug :: Expr -> Context -> Expr
plug r (AppL c e t) = App (plug r c) e t
plug r (AppR e c t) = App e (plug r c) t
plug r (Hole _)     = r

concretize :: Int -> Expr -> Expr
concretize i e = App e (obj i) (Just $ TyCon "t")

class Reducible e where
  reduce :: Env -> CtEnv -> Size -> e -> Error e

--  , ("NOT"     , t ~> t)
--  , ("AND"     , t ~> (t ~> t))
--  , ("OR"      , t ~> (t ~> t))
--  , ("IMPLIES" , t ~> (t ~> t))
--  , ("SEILPMI" , t ~> (t ~> t))
--  , ("EQUIV"   , t ~> (t ~> t))
--  , ("EQUALS"  , e ~> (e ~> t))
--  , ("IOTA"    , (e ~> t) ~> e)
--  , ("FORALL"  , (e ~> t) ~> t)
--  , ("EXISTS"  , (e ~> t) ~> t)

prim :: String -> Expr
prim n = Var n (Just (stdTyEnv ! n))

fun1 :: String -> Expr -> Expr
fun1 n e = App (prim n) e (Just b)
  where
    (TyArr _ b) = stdTyEnv ! n

fun2 :: String -> Expr -> Expr -> Expr
fun2 n e1 e2 = App (App (prim n) e1 (Just b2c)) e2 (Just c)
  where
    (TyArr _ b2c@(TyArr _ c)) = stdTyEnv ! n

primBOOL :: Bool -> Expr
primBOOL B.True  = prim "TRUE"
primBOOL B.False = prim "FALSE"

primNOT :: (Expr -> Error Expr) -> Expr -> Error Expr
primNOT cont e1 =
  do r1 <- cont e1
     case r1 of
       Var "FALSE" _ -> return (prim "TRUE")
       Var "TRUE"  _ -> return (prim "FALSE")
       _             -> return (fun1 "NOT" r1)

primAND :: (Expr -> Error Expr) -> Expr -> Expr -> Error Expr
primAND cont e1 e2 =
  do r1 <- cont e1
     case r1 of
       Var "FALSE" _ -> return (prim "FALSE")
       Var "TRUE"  _ ->
         do r2 <- cont e2
            case r2 of
              Var "FALSE" _ -> return (prim "FALSE")
              Var "TRUE"  _ -> return (prim "TRUE")
              _             -> return (fun2 "AND" r1 r2)
       _             -> return (fun2 "AND" r1 e2)

primEQUALS :: (Expr -> Error Expr) -> Expr -> Expr -> Error Expr
primEQUALS cont e1 e2 =
  do r1 <- cont e1
     r2 <- cont e2
     case (r1,r2) of
       (Obj i _ , Obj j _) -> return (primBOOL (i == j))
       _                   -> return (fun2 "EQUALS" r1 r2)

primIOTA :: (Expr -> Error Expr) -> Size -> Expr -> Error Expr
primIOTA cont size e =
  do es <- mapM cont [ concretize o e | o <- [0 .. size - 1]]
     let is0 = zip [0 ..] es
     let is1 = filter (isTRUE . snd) is0
     case is1 of
       [ p ] -> return (obj (fst p))
       _     -> fail ("IOTA: non-unique element in evaluating " ++ show e)

rewriteOR :: Expr -> Expr -> Expr
rewriteOR e1 e2 = fun1 "NOT" (fun2 "AND" (fun1 "NOT" e1) (fun1 "NOT" e2))

rewriteIMPLIES :: Expr -> Expr -> Expr
rewriteIMPLIES e1 e2 = fun2 "OR" (fun1 "NOT" e1) e2

rewriteSEILPMI :: Expr -> Expr -> Expr
rewriteSEILPMI e1 e2 = rewriteIMPLIES e2 e1

rewriteEQUIV :: Expr -> Expr -> Expr
rewriteEQUIV e1 e2 = fun2 "AND" (rewriteIMPLIES e1 e2) (rewriteSEILPMI e1 e2)

rewriteFORALL :: Size -> Expr -> Expr
rewriteFORALL size e =
  foldr1 (fun2 "AND") [ concretize o e | o <- [0 .. size - 1]]

rewriteEXISTS :: Size -> Expr -> Expr
rewriteEXISTS size e =
  foldr1 (fun2 "OR") [ concretize o e | o <- [0 .. size - 1]]

isTRUE :: Expr -> Bool
isTRUE (Var "TRUE" _) = B.True
isTRUE _ = B.False

instance Reducible Expr where
  reduce env ctenv d = reduce'
    where
      -- Reduction rules for primitive functions
      reduce' (App (Var "NOT" _) e1 _)                = primNOT reduce' e1
      reduce' (App (App (Var "AND" _) e1 _) e2 _)     = primAND reduce' e1 e2
      reduce' (App (App (Var "EQUALS" _) e1 _) e2 _)  = primEQUALS reduce' e1 e2
      reduce' (App (App (Var "OR" _) e1 _) e2 _)      = reduce' $ rewriteOR e1 e2
      reduce' (App (App (Var "IMPLIES" _) e1 _) e2 _) = reduce' $ rewriteIMPLIES e1 e2
      reduce' (App (App (Var "SEILPMI" _) e1 _) e2 _) = reduce' $ rewriteSEILPMI e1 e2
      reduce' (App (App (Var "EQUIV" _) e1 _) e2 _)   = reduce' $ rewriteEQUIV e1 e2
      reduce' (App (Var "FORALL" _) e1 _)             = reduce' $ rewriteFORALL d e1
      reduce' (App (Var "EXISTS" _) e1 _)             = reduce' $ rewriteEXISTS d e1
      reduce' (App (Var "IOTA" _) e1 _)               = primIOTA reduce' d e1

      -- Beta reduction
      reduce' (App (Abs n e2 _) e1 _) = reduce' (apply (Subst n e1) e2)

      -- Charasteristic function application (set reduction)
      reduce' (App (Set es _) e _) = return (primBOOL (e `elem` es))

      -- Call by name delayed reduction
      reduce' (App e1 e2 t) = do
        r1 <- reduce env ctenv d e1
        let a = App r1 e2 t
        case r1 of
          Abs {} -> reduce' a
          Set {} -> reduce' a
          _      -> return  a

      -- Simple forwarding rules
      reduce' (Abs n e t)  = do e' <- reduce' e; return (Abs n e' t)
      reduce' (Var n _)    = do e' <- find n env; reduce' e'
      reduce' o@(Obj _ _)  = return o
      reduce' (Set es t)   = do es' <- mapM reduce' es; return (Set es' t)
      reduce' (Plug e c _) = do c' <- reduce env ctenv d c; reduce' (plug e c')

instance Reducible Context where
  reduce env ctenv d = reduce'
    where
      reduce' (CVar n _)   = do
        c' <- find n ctenv; reduce' c'
      reduce' (AppL c e t) = do
        c' <- reduce' c; e' <- reduce env ctenv d e; return (AppL c' e' t)
      reduce' (AppR e c t) = do
        e' <- reduce env ctenv d e; c' <- reduce' c; return (AppR e' c' t)
      reduce' h@(Hole _)   = return h

eval :: Size -> [Decl] -> Error (Env , CtEnv , TyEnv)
eval d ds = evalAcc (M.empty , M.empty , M.empty) ds
  where
    evalAcc :: (Env , CtEnv , TyEnv) -> [Decl] -> Error (Env , CtEnv , TyEnv)
    evalAcc (env , ctenv , tyenv) []
      = return (env , ctenv , tyenv)
    evalAcc (env , ctenv , tyenv) (Expr n (Just t) e : ds)
      = do e' <- reduce env ctenv d e
           evalAcc (M.insert n e' env , ctenv , M.insert n t tyenv) ds
    evalAcc (env , ctenv , tyenv) (Context n (Just t) c : ds)
      = do c' <- reduce env ctenv d c
           evalAcc (env , M.insert n c' ctenv , M.insert n t tyenv) ds
