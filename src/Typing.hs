module Typing where



import Base
import Error
import Substitution
import Unification
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.Map (Map,(!))
import qualified Data.Map as M (insert,lookup,unions)
import Text.Printf (PrintfArg,printf)




class Infer e where
  infer :: TyEnv -> e -> Error (e , Type , TyEnv , [TySubst])

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
  infer env0 (Pair e1 e2 tyAnn)   = inferPair env0 e1 e2 tyAnn Pair
  infer env0 (Case n1 n2 e tyAnn) = inferCase env0 n1 n2 e tyAnn Case
  infer env0 (Obj i tyAnn)        = inferObj env0 i tyAnn Obj
  infer env0 (Set es tyAnn)       = inferSet env0 es tyAnn Set
  infer env0 (Hole (Just t))      = return (Hole (Just t) , t , env0 , [])
  infer  _   (Hole Nothing)       = fail "Untyped holes are unsupported"

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

inferPair env0 e1 e2 tyAnn mkpair = do
  (e1' , t1 , env1 , s0) <- infer env0 e1
  (e2' , t2 , env2 , s1) <- infer env1 e2
  s2 <- unifyIn t1 t2 (show $ mkpair e1 e2 tyAnn)
  let t1'  = apply s2 t1
  let t2'  = apply s2 t2
  let e1'' = apply s2 e1'
  let e2'' = apply s2 e2'
  let t    = TyPair t1' t2'
  (r , s3) <- checkTyAnn tyAnn t
  let s    = s3 ++ s2 ++ s1 ++ s0
  let env3 = apply s env2
  return (mkpair e1'' e2'' (Just r) , r , env3 , s)

inferCase env0 n1 n2 e tyAnn mkcase = do
  a <- freshTyVar
  b <- freshTyVar
  (e' , t , env1 , s0) <- infer (M.insert n2 b . M.insert n1 a $ env0) e
  (r , s1) <- checkTyAnn tyAnn ((apply s0 $ a `TyPair` b) `TyArr` t)
  let s    = s1 ++ s0
  let env2 = apply s env1
  return (mkcase n1 n2 e' (Just r) , r , env2 , s)

inferObj env0 i tyAnn mkobj = do
  let t = TyCon "e"
  (t' , s) <- checkTyAnn tyAnn t
  let env1 = apply s env0
  return (mkobj i (Just t') , t' , env1 , s)

inferSet env0 es tyAnn mkset = do
  (es' , t : ts , envs , ss) <- split4 <$> mapM (infer env0) es
  let ctxt = show (mkset es tyAnn)
  (t' , s0) <- foldM (\a b -> unifyIn' a b ctxt) (t , []) ts
  let es'' = map (\e -> annotate e (Just t')) es'
  let t''  = TyArr t' (TyCon "t")
  (t''' , s1) <- checkTyAnn tyAnn t''
  let s    = s1 ++ s0 ++ concat ss
  let env1 = apply s (M.unions envs)
  return (mkset es'' (Just t''') , t''' , env1 , s)
  where
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
  s2 <- unifyIn t1 t2 (show $ mkplug e c tyAnn)
  let t    = apply s2 t2
  let e''  = apply s2 e'
  let c''  = apply s2 c'
  (r , s3) <- checkTyAnn tyAnn t
  let s    = s3 ++ s2 ++ s1 ++ s0
  let env3 = apply s env2
  return (mkplug e'' c'' (Just r) , r , env3 , s)


checkTyAnn :: TyAnn -> Type -> Error (Type , [TySubst])
checkTyAnn  Nothing  t2 = return (t2 , [])
checkTyAnn (Just t1) t2 = do s <- unify t1 t2; return (apply s t2 , s)
