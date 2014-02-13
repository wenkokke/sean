module Reducing where

import Prelude hiding (True,False,abs,and,or)
import Base
import Error
import Substitution
import qualified Data.Bool as B (Bool (True,False))
import qualified Data.Map as M (lookup,empty,insert)

-- * Evaluation Strategies

type Size = Int

concretize :: Int -> Expr -> Expr
concretize i e = App e (obj i) (Just $ TyCon "t")

class Reducible e where
  reduce :: Env -> Size -> e -> Error e

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
       _     -> fail ("IOTA: non-unique object for predicate, in " ++ show e)

rewriteOR :: Expr -> Expr -> Expr
rewriteOR e1 e2 = fun1 "NOT" (fun2 "AND" (fun1 "NOT" e1) (fun1 "NOT" e2))

rewriteIMPLIES :: Expr -> Expr -> Expr
rewriteIMPLIES e1 e2 = fun2 "OR" (fun1 "NOT" e1) e2

rewriteEQUIV :: Expr -> Expr -> Expr
rewriteEQUIV e1 e2 = fun2 "AND" (rewriteIMPLIES e1 e2) (rewriteIMPLIES e2 e1)

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
  reduce env d = reduce'
    where
      -- Reduction rules for primitive functions
      reduce' (App (Var "NOT" _) e1 _)                = primNOT reduce' e1
      reduce' (App (App (Var "AND" _) e1 _) e2 _)     = primAND reduce' e1 e2
      reduce' (App (App (Var "EQUALS" _) e1 _) e2 _)  = primEQUALS reduce' e1 e2
      reduce' (App (App (Var "OR" _) e1 _) e2 _)      = reduce' $ rewriteOR e1 e2
      reduce' (App (App (Var "IMPLIES" _) e1 _) e2 _) = reduce' $ rewriteIMPLIES e1 e2
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
        r1 <- reduce' e1
        let a = App r1 e2 t
        case r1 of
          Abs {} -> reduce' a
          Set {} -> reduce' a
          _      -> return  a

      -- Simple forwarding rules
      reduce' (Abs n e t)  = do e' <- reduce' e; return (Abs n e' t)
      reduce' v@(Var n _)  = case M.lookup n env of
        Just e' -> reduce' e'
        Nothing -> return v
      reduce' o@(Obj _ _)  = return o
      reduce' (Set es t)   = do es' <- mapM reduce' es; return (Set es' t)
      reduce' (Plug e c _) = do c' <- reduce' c; reduce' (plug e c')
      reduce' h@(Hole _)   = return h

plug :: Expr -> Expr -> Expr
plug _ v@(Var _ _)    = v
plug _ o@(Obj _ _)    = o
plug e (Abs x e1 t)   = Abs x (plug e e1) t
plug e (App e1 e2 t)  = App (plug e e1) (plug e e2) t
plug e (Set xs t)     = Set (map (plug e) xs) t
plug e (Hole t)       = e
plug _ p@(Plug _ _ _) = p


eval :: Size -> [Decl] -> Error (Env , TyEnv)
eval d = evalAcc (M.empty , M.empty)
  where
    evalAcc :: (Env , TyEnv) -> [Decl] -> Error (Env , TyEnv)
    evalAcc (env , tyenv) []
      = return (env , tyenv)
    evalAcc (env , tyenv) (Decl n (Just t) e : ds)
      = do e' <- reduce env d e
           evalAcc (M.insert n e' env , M.insert n t tyenv) ds
