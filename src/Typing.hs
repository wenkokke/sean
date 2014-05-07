{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Typing where



import Base
import Error
import Substitution
import Unification
import Control.Applicative ((<$>))
import Control.Monad.Trans.Except (throwE)
import Control.Monad (foldM)
import Data.Map (Map,(!))
import qualified Data.Map as M (insert,lookup,unions)
import qualified Data.Maybe as M
import Text.Printf (PrintfArg,printf)




class Infer e where
  infer :: TyEnv -> e -> Error (e , Type , TyEnv , [TySubst])

instance Infer (Expr,Expr) where
  infer env0 (e1,e2) = do
    (e1' , t1 , env1 , s0) <- infer env0 e1
    (e2' , t2 , env2 , s1) <- infer env1 e2
    let (e1'' , t1') = (apply s1 e1' , apply s1 t1)
    return ((e1'',e2') , TyPair t1' t2 , env2 , s1 ++ s0)

instance Infer (Expr,Expr,Expr) where
  infer env0 (e1,e2,e3) = do
    (e1' , t1 , env1 , s0) <- infer env0 e1
    (e2' , t2 , env2 , s1) <- infer env1 e2
    (e3' , t3 , env3 , s2) <- infer env2 e3
    let s = s2 ++ s1 ++ s0
    let (e1'' , t1') = (apply s e1' , apply s t1)
    let (e2'' , t2') = (apply s e2' , apply s t2)
    return ((e1'', e2'', e3') , TyPair t1' (TyPair t2' t3) , env3 , s2 ++ s1 ++ s0)

instance Infer Decl where
  infer env0 (Decl n tyAnn e) = do
    (e' , t , env1 , s0) <- infer env0 e
    (r , s1) <- checkTyAnn tyAnn t
    return (Decl n (Just r) e' , r , env1 , s1 ++ s0)

instance Infer Expr where
  infer env0 (Var n tyAnn)        = inferVar env0 n tyAnn Var
  infer env0 (Abs n x tyAnn)      = inferAbs env0 n x tyAnn Abs
  infer env0 (App f x tyAnn)      = inferApp env0 f x tyAnn App
  infer env0 (Plug e c tyAnn)     = inferPlug env0 e c tyAnn Plug
  infer env0 (Obj i tyAnn)        = inferObj env0 i tyAnn Obj
  infer env0 (Rel1 es tyAnn)      = inferRelN env0 es tyAnn Rel1
  infer env0 (Rel2 es tyAnn)      = inferRelN env0 es tyAnn Rel2
  infer env0 (Rel3 es tyAnn)      = inferRelN env0 es tyAnn Rel3
  infer env0 (Hole (Just t))      = return (Hole (Just t) , t , env0 , [])
  infer  _   (Hole Nothing)       = throwE "Untyped holes are unsupported"

-- TODO Allow variable case to bind a fresh type variable in the case of
--      unbound identifiers, and propagate these upwards. This is usefull
--      to allow expressions such as "ix.dog:et x" without first having to
--      define "dog", and to allow untyped holes.
inferVar env0 n tyAnn mkvar =
  case M.lookup n env0 of
    Just e ->
      do (r , s) <- checkTyAnn tyAnn e
         let env1 = apply s env0
         return (mkvar n (Just r) , r , env1 , s)
    Nothing ->
      case tyAnn of
        Just r  -> return (mkvar n (Just r) , r , env0 , [])
        Nothing ->
          do r <- freshTyVar
             let env1 = M.insert n r env0
             return (mkvar n (Just r) , r , env1 , [])

inferAbs env0 n x tyAnn mkabs = do
  a <- freshTyVar
  (x' , b , env1 , s0) <- infer (M.insert n a env0) x
  (r , s1) <- checkTyAnn tyAnn (apply s0 a `TyArr` b)
  let s    = s1 ++ s0
  let env2 = apply s env1
  return (mkabs n x' (Just r) , r , env2 , s)

inferApp env0 f x tyAnn mkapp = do
  (f' , a2b , env1 , s0) <- infer env0 f
  (x' , a   , env2 , s1) <- infer (apply s0 env1) x
  b <- freshTyVar
  s2 <- unifyIn (apply s1 a2b) (a `TyArr` b) (show $ mkapp f' x' tyAnn)
  let f''  = apply s2 f'
  let x''  = apply s2 x'
  let b'   = apply s2 b
  (r , s3) <- checkTyAnn tyAnn b'
  let s    = s3 ++ s2 ++ s1 ++ s0
  let env3 = apply s env2
  return (mkapp f'' x'' (Just r) , r , env3 , s)

inferObj env0 i tyAnn mkobj = do
  let t = TyCon "e"
  (t' , s) <- checkTyAnn tyAnn t
  let env1 = apply s env0
  return (mkobj i (Just t') , t' , env1 , s)

inferRelN env0 es tyAnn mkset = do
  (es' , t : ts , envs , ss) <- split4 <$> mapM (infer env0) es
  let ctxt = show (mkset es tyAnn)
  (t' , s0) <- foldM (\a b -> unifyIn' a b ctxt) (t , []) ts
  let es'' = map (\e -> annotate e (Just t')) es'
  let t''  = tyCurry t' (TyCon "t")
  (t''' , s1) <- checkTyAnn tyAnn t''
  let s    = s1 ++ s0 ++ concat ss
  let env1 = apply s (M.unions envs)
  return (mkset es'' (Just t''') , t''' , env1 , s)
  where
    tyCurry :: Type -> Type -> Type
    tyCurry (TyPair a b) r = TyArr a (tyCurry b r)
    tyCurry a r = TyArr a r

    split4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
    split4 [] = ([] , [] , [], [])
    split4 ((a,b,c,d) : rs) = case split4 rs of
      (as , bs , cs , ds) -> (a : as , b : bs , c : cs , d : ds)

    unifyIn' :: PrintfArg a => (Type,[TySubst]) -> Type -> a -> Error (Type,[TySubst])
    unifyIn' (a,s0) b e = do
      let b' = apply s0 b
      s1 <- unifyIn a b' e
      let a' = apply s1 a
      return (a' , s1 ++ s0)

inferPlug env0 e c tyAnn mkplug = do
  (e' , t1 , env1 , s0) <- infer env0 e
  (c' , t2 , env2 , s1) <- infer env1 c
  let t3 = getHoleType c'
  if (M.isJust t3)
    then do
    s2 <- unifyIn t1 (M.fromJust t3) (show $ mkplug e c tyAnn)
    let t    = apply s2 t2
    let e''  = apply s2 e'
    let c''  = apply s2 c'
    (r , s3) <- checkTyAnn tyAnn t
    let s    = s3 ++ s2 ++ s1 ++ s0
    let env3 = apply s env2
    return (mkplug e'' c'' (Just r) , r , env3 , s)
    else do
    (r , s2) <- checkTyAnn tyAnn t2
    let s    = s2 ++ s1 ++ s0
    let env3 = apply s env2
    return (mkplug e' c' (Just r) , r , env3 , s)


checkTyAnn :: TyAnn -> Type -> Error (Type , [TySubst])
checkTyAnn  Nothing  t2 = return (t2 , [])
checkTyAnn (Just t1) t2 = do s <- unify t1 t2; return (apply s t2 , s)
