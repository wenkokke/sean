module Resolving where

import Base
import Error
import Typing
import Unification
import Substitution
import Control.Arrow (second)
import Control.Applicative ((<$>),(<*>))
import Control.Monad (when)
import Data.Char (isDigit,digitToInt)
import Data.Either (lefts)
import Data.Maybe (fromJust)
import qualified Data.Map as M (insert)
import qualified Data.List as L (or)


-- * Global Constants

labelSep :: Char
labelSep = '#'


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
ident (Decl n t _) = (n , t)

class FreeIdents e where
  freeIdents :: e -> [(Name , TyAnn)]

instance FreeIdents Decl where
  freeIdents (Decl _ _ e) = freeIdents e

instance FreeIdents Expr where
  freeIdents (Var n t)      = [(n , t)]
  freeIdents (Abs n1 e t1)  = filter (not . compatible (n1,t1)) (freeIdents e)
  freeIdents (App e1 e2 _)  = freeIdents e1 ++ freeIdents e2
  freeIdents (Set es _)     = concatMap freeIdents es
  freeIdents (Obj _ _)      = []
  freeIdents (Hole _)       = []
  freeIdents (Plug e c _)   = freeIdents e  ++ freeIdents c

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
  resolve _ (Hole t) =
    return (Hole t)

  -- resolution rules for objects and sets
  resolve _ o@(Obj _ _)   = return o
  resolve seen (Set es t) = Set <$> mapM (resolve seen) es <*> return t

instance Resolvable Decl where
  resolve seen (Decl n t e) =
    [ Decl    n t e' | e' <- resolve seen e ]

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
        wf <- map fst4 <$> sequence (filter (not . isError) typed)

        let wf'      = zipWith (rename . relabel) [i0 ..] wf
        let seen'    = seen ++ (ident <$> wf)
        let amb'     = ident d : amb
        let acc'     = wf' ++ acc
        let env'     = foldr (uncurry M.insert . second fromJust) env (ident <$> wf')

        disambiguate' seen' amb' acc' env' ds

      | otherwise = do
        (d' , t , _ , _) <- infer env d

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

        fst4 :: (a,b,c,d) -> a
        fst4 (x,_,_,_) = x

rename :: (Name -> Name) -> Decl -> Decl
rename f (Decl n t e) = Decl (f n) t e

relabel :: Int -> Name -> Name
relabel i = joinLabel . second (++ [i]) . splitLabel
  where
    splitLabel :: Name -> (Name,Label)
    splitLabel = second (map digitToInt . filter isDigit) . break (==labelSep)
    joinLabel  :: (Name,Label) -> Name
    joinLabel  = (\(n,ls) -> n ++ labelSep : ls) . second (concatMap show)


compatible :: (Name , TyAnn) -> (Name , TyAnn) -> Bool
compatible (n1 , t1) (n2 , t2) = n1 == n2 && compTyAnn t1 t2
  where
    compTyAnn :: TyAnn -> TyAnn -> Bool
    compTyAnn  Nothing    _         = True
    compTyAnn  _          Nothing   = True
    compTyAnn (Just t1') (Just t2') = unifiable t1' t2'

(><) :: TyAnn -> TyAnn -> TyAnn
Nothing >< tyAnn = tyAnn
tyAnn >< Nothing = tyAnn
(Just t1)  >< (Just t2) = noerror
  where
    noerror = either (const Nothing) Just (supplyFreshNames unified)
    unified = do s <- unify t1 t2; return (apply s t1)
