{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, RankNTypes #-}

module SeAn.Base where

import Prelude hiding (abs)
import Control.Arrow (second)
import Control.Applicative ((<$),(<$>),(<|>),(<*>))
import Control.Monad (when)
import Control.Monad.Error (ErrorT,runErrorT,mapErrorT,lift)
import Control.Monad.Supply
import Data.Char (isDigit,digitToInt)
import Data.Either (lefts)
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.ParserCombinators.UU ((<?>),(<??>),pList1,pList1Sep,pMaybe,pChainl)
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils
import Text.Printf (printf,PrintfArg)

type Name  = String
type Label = [Int]


run :: [String] -> Either String [Decl]
run = supplyFreshNames . compile . map (runParser "string" pDecl)

-- * Abstract Syntax Trees

data Type where
  TyCon :: Name -> Type
  TyVar :: Name -> Type
  TyArr :: Type -> Type -> Type
  deriving (Eq,Show)

type TyAnn = Maybe Type

data Decl where
  Expr :: Name -> TyAnn -> Expr -> Decl
  Context :: Name -> TyAnn -> Context -> Decl
  deriving (Eq,Show)

data Expr where
  Var  :: Name -> TyAnn -> Expr
  Abs  :: Name -> Expr -> TyAnn -> Expr
  App  :: Expr -> Expr -> TyAnn -> Expr
  Plug :: Expr -> Context -> TyAnn -> Expr
  deriving (Eq,Show)

data Context where
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
  setTyAnn (Var n t1) t2 = Var n (t2 <|> t1)
  setTyAnn (Abs n e t1) t2 = Abs n e (t2 <|> t1)
  setTyAnn (App e1 e2 t1) t2 = App e1 e2 (t2 <|> t1)

instance HasTyAnn Context where
  setTyAnn (AppL c e t1) t2 = AppL c e (t2 <|> t1)
  setTyAnn (AppR e c t1) t2 = AppR e c (t2 <|> t1)
  setTyAnn (Hole t1) t2 = Hole (t2 <|> t1)

-- ** Parsing names

pName  :: Parser Name
pName  = lexeme (pList1 pAlphaNum_) <?> "name"

pNames :: Parser [Name]
pNames = pList1Sep pSpaces pName

pAlphaNum_ :: Parser Char
pAlphaNum_ = pLetter <|> pDigit <|> pSym '_'

-- ** Parsing expressions

pExpr :: Parser Expr
pExpr = pChainl (app <$ pSpaces) pExprAtom

pVar :: Parser Expr
pVar = iI Var pName (pMaybe pTyAnn) Ii

pGGQ :: Parser Expr
pGGQ  = pAbs <|> pUniv <|> pExis <|> pIota
  where
    pAbs  = iI absn '\\' pNames '.' pExpr Ii <?> "lambda abstraction"
    pUniv = iI univ '!'  pNames '.' pExpr Ii <?> "universal quantifier"
    pExis = iI exis '?'  pNames '.' pExpr Ii <?> "existential quantifier"
    pIota = iI iota 'i'  pNames '.' pExpr Ii <?> "iota"

pExprAtom :: Parser Expr
pExprAtom  = pVar <|> pGGQ <|> (pParens pExpr <??> pOptTyAnn)

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

var :: Name -> Expr
var n = Var n Nothing

abs :: Name -> Expr -> Expr
abs n e = Abs n e Nothing

absn :: [Name] -> Expr -> Expr
absn xs e = foldr abs e xs

app :: Expr -> Expr -> Expr
app e1 e2 = App e1 e2 Nothing

appl :: Context -> Expr -> Context
appl e1 e2 = AppL e1 e2 Nothing

appr :: Expr -> Context -> Context
appr e1 e2 = AppR e1 e2 Nothing

ggq :: Name -> [Name] -> Expr -> Expr
ggq gq xs e = foldr ((app (Var gq (M.lookup gq stdEnv)) .) . abs) e xs

univ :: [Name] -> Expr -> Expr
univ = ggq "FORALL"

exis :: [Name] -> Expr -> Expr
exis = ggq "EXISTS"

iota :: [Name] -> Expr -> Expr
iota = ggq "IOTA"



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
  freeIdents (Var n t) = [(n , t)]
  freeIdents (Abs n1 e t1) = L.filter (not . compatible (n1,t1)) (freeIdents e)
  freeIdents (App e1 e2 _) = freeIdents e1 ++ freeIdents e2

instance FreeIdents Context where
  freeIdents (AppL e1 e2 _) = freeIdents e1 ++ freeIdents e2
  freeIdents (AppR e1 e2 _) = freeIdents e1 ++ freeIdents e2
  freeIdents (Hole _) = []

stdEnv :: TyEnv
stdEnv = M.fromList
  [ ("~"      , t ~> t)
  , ("/\\"    , t ~> (t ~> t))
  , ("\\/"    , t ~> (t ~> t))
  , ("=>"     , t ~> (t ~> t))
  , ("<="     , t ~> (t ~> t))
  , ("<=>"    , t ~> (t ~> t))
  , ("=="     , e ~> (e ~> t))
  , ("IOTA"   , (e ~> t) ~> e)
  , ("FORALL" , (e ~> t) ~> t)
  , ("EXISTS" , (e ~> t) ~> t)
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
      comp = filter (compatible (n1,t1)) poss
      poss = indexed n1 seen

instance Resolvable Context where
  resolve seen (AppL c e t) =
    [ AppL c' e' t | c' <- resolve seen c, e' <- resolve seen e ]
  resolve seen (AppR e c t) =
    [ AppR e' c' t | e' <- resolve seen e, c' <- resolve seen c ]
  resolve _ (Hole t) =
    return (Hole t)

instance Resolvable Decl where
  resolve seen (Expr n t e) =
    [ Expr n t e' | e' <- resolve seen e ]
  resolve seen (Context n t c) =
    [ Context n t c' | c' <- resolve seen c ]

ambError :: [Error a] -> Error ()
ambError xs = fail msg
  where
    msg
      | length msgs == 1 = head msgs
      | otherwise       = "Ambiguous type error:\n" ++ unlines msgs
    msgs :: [String]
    msgs = lefts (supplyFreshNames <$> xs)

compile :: [Decl] -> Error [Decl]
compile = compile' [] [] [] stdEnv

compile' :: [(Name,TyAnn)] -> [(Name,TyAnn)] -> [Decl] -> TyEnv -> [Decl] -> Error [Decl]
compile' _ _ acc _ [] = return (reverse acc)
compile' seen amb acc env (d:ds)
  | cond1 || cond2 = do

    let i0       = count n seen
    let resolved = resolve seen d
    let typed    = map (infer stdEnv) resolved

    when (all isError typed) (ambError typed)
    wf <- map fst3 <$> sequence (filter (not . isError) typed)

    let wf'      = zipWith (rename . relabel) [i0 ..] wf
    let seen'    = seen ++ (ident <$> wf)
    let amb'     = ident d : amb
    let acc'     = wf' ++ acc
    let env'     = foldr (uncurry M.insert . second fromJust) env (ident <$> wf')

    compile' seen' amb' acc' env' ds
  | otherwise = do
    (d' , t , _) <- infer env d

    let seen' = seen ++ [ident d']
    let acc'  = d' : acc
    let env'  = M.insert n t env

    compile' seen' amb acc' env' ds

  where
    i = ident d
    n = fst i
    used  = freeIdents d
    cond1 = or [ n == m | (m,_) <- seen ]
    cond2 = or [ n == m | (n,_) <- used, (m,_) <- amb ]

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
  apply s (Var n t) = Var n (apply s t)
  apply s (Abs n e t) = Abs n (apply s e) (apply s t)
  apply s (App e1 e2 t) = App (apply s e1) (apply s e2) (apply s t)

instance Apply TySubst Context where
  apply s (AppL c e t) = AppL (apply s c) (apply s e) (apply s t)
  apply s (AppR e c t) = AppR (apply s e) (apply s c) (apply s t)
  apply s (Hole t) = Hole (apply s t)

instance Apply TySubst (Map Name Type) where
  apply s = M.map (apply s)

data Subst where
  Subst :: Name -> Expr -> Subst

-- TODO Substitution is not type-safe.
instance Apply Subst Expr where
  apply (Subst n1 r) = subst
    where
      subst v@(Var n2 _) = if n1 == n2 then r else v
      subst a@(Abs n2 e t) = if n1 == n2 then a else Abs n2 (subst e) t
      subst (App e1 e2 t) = App (subst e1) (subst e2) t



-- * Type Inference

type TyEnv = Map Name Type

type Error a = ErrorT String (Supply Name) a

isError :: Error a -> Bool
isError = either (const True) (const False) . supplyFreshNames

supplyFreshNames :: Error a -> Either String a
supplyFreshNames e = evalSupply (runErrorT e) freshNames

freshNames :: [Name]
freshNames = letters ++ numbers
  where
  letters    = fmap (: []) (['A'..'D'] ++ ['F'..'S'] ++ ['U'..'Z'])
  numbers    = fmap (('T' :) . show) [0 ..]

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
  infer env (Var n tyAnn) = inferVar env n tyAnn Var
  infer env (Abs n x tyAnn) = inferAbs env n x tyAnn Abs
  infer env (App f x tyAnn) = inferApp env f x tyAnn App
  infer env (Plug e c tyAnn) = inferPlug env e c tyAnn Plug

instance Infer Context where
  infer env (AppL c e tyAnn) = inferApp env c e tyAnn AppL
  infer env (AppR e c tyAnn) = inferApp env e c tyAnn AppR
--infer env (Hole tyAnn)     = inferVar env holeName tyAnn (\_ t -> Hole t)
  infer  _  (Hole (Just t))  = return (Hole (Just t) , t , [])
  infer  _  (Hole Nothing)   = fail "Untyped holes are currently unsupported"

-- TODO Allow variable case to bind a fresh type variable in the case of
--      unbound identifiers, and propagate these upwards. This is usefull
--      to allow expressions such as "ix.dog:et x" without first having to
--      define "dog", and to allow untyped holes.
inferVar env n tyAnn var = do
  a <- maybe (fail (printf "Unbound identifier %s" n)) return (M.lookup n env)
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

checkTyAnn :: TyAnn -> Type -> Error (Type , [TySubst])
checkTyAnn  Nothing  t2 = return (t2 , [])
checkTyAnn (Just t1) t2 = do s <- unify t1 t2; return (apply s t2 , s)

compatible :: (Name , TyAnn) -> (Name , TyAnn) -> Bool
compatible (n1 , t1) (n2 , t2) = n1 == n2 && compTyAnn t1 t2
  where
    compTyAnn :: TyAnn -> TyAnn -> Bool
    compTyAnn  Nothing   _        = True
    compTyAnn  _         Nothing  = True
    compTyAnn (Just t1) (Just t2) = unifiable t1 t2

(><) :: TyAnn -> TyAnn -> TyAnn
Nothing >< tyAnn = tyAnn
tyAnn >< Nothing = tyAnn
(Just t1)  >< (Just t2) = noerror
  where
    noerror = either (const Nothing) (Just) (supplyFreshNames unified)
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
