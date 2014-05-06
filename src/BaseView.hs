{-# LANGUAGE GADTs #-}
module SeAn.BaseView where

import qualified Data.List as L (intercalate)
import Text.Printf (printf)

type Name = String

data Type where

  TyCon  :: Name -> Type
  TyVar  :: Name -> Type
  TyArr  :: Type -> Type -> Type
  TyPair :: Type -> Type -> Type

  deriving (Eq, Show)

type TyAnn = Maybe Type

data ExprView where

  -- function types
  Var     :: Name -> TyAnn -> ExprView
  Abs     :: Name -> ExprView -> TyAnn -> ExprView
  App     :: ExprView -> ExprView -> TyAnn -> ExprView

  -- pairs
  Pair    :: ExprView -> ExprView -> TyAnn -> ExprView
  Fst     :: ExprView -> TyAnn -> ExprView
  Snd     :: ExprView -> TyAnn -> ExprView

  -- sets and objects
  Obj     :: Int -> TyAnn -> ExprView
  Set     :: [ExprView] -> TyAnn -> ExprView

  -- quantifiers, predicates and truth values
  Iota    :: Name -> ExprView -> TyAnn -> ExprView
  Forall  :: Name -> ExprView -> TyAnn -> ExprView
  Exists  :: Name -> ExprView -> TyAnn -> ExprView

  Not     :: ExprView -> TyAnn -> ExprView
  And     :: ExprView -> ExprView -> TyAnn -> ExprView
  Or      :: ExprView -> ExprView -> TyAnn -> ExprView
  Implies :: ExprView -> ExprView -> TyAnn -> ExprView
  Equiv   :: ExprView -> ExprView -> TyAnn -> ExprView
  Equals  :: ExprView -> ExprView -> TyAnn -> ExprView

  Yes     :: TyAnn -> ExprView
  No      :: TyAnn -> ExprView

  -- holes and plugs
  Hole    :: Type -> ExprView
  Plug    :: ExprView -> ExprView -> TyAnn -> ExprView

  deriving (Eq)

isPair :: ExprView -> Bool
isPair (Pair {}) = True
isPair _ = False

isApp :: ExprView -> Bool
isApp (App {}) = True
isApp _ = False

isComplex :: ExprView -> Bool
isComplex (Pair {})    = True
isComplex (App {})     = True
isComplex (Not {})     = True
isComplex (And {})     = True
isComplex (Or {})      = True
isComplex (Implies {}) = True
isComplex (Equiv {})   = True
isComplex (Equals {})  = True



-- * Instances

instance Show ExprView where
  show expr = case expr of
    Var n  Nothing  -> n
    Var n (Just ty) -> printf "%s:%s" n (show ty)
    Abs n e1 _      -> printf "\\%s.%s" n (show e1)
    App e1 e2 _     -> printf "%s %s" (parensIf isComplex e1) (show e2)

    Pair e1 e2 _    -> printf "%s, %s" (parensIf isPair e1) (show e2)
    Fst e1 _        -> printf "fst %s" (parensIf isComplex e1)
    Snd e1 _        -> printf "snd %s" (parensIf isComplex e1)

    Obj e1 _        -> show e1
    Set ls _        -> printf "{%s}" (L.intercalate "," $ map (parensIf isPair) ls)

    Iota n e1 _     -> printf "i%s.%s" n (show e1)
    Forall n e1 _   -> printf "!%s.%s" n (show e1)
    Exists n e1 _   -> printf "?%s.%s" n (show e1)

    Not e1 _        -> printf "~%s" (parensIf isComplex e1)
    And e1 e2 _     -> printf "%s /\\ %s" (parensIf isComplex e1) (parensIf isComplex e2)
    Or e1 e2 _      -> printf "%s \\/ %s" (parensIf isComplex e1) (parensIf isComplex e2)
    Implies e1 e2 _ -> printf "%s => %s" (parensIf isComplex e1) (parensIf isComplex e2)
    Equiv e1 e2 _   -> printf "%s <=> %s" (parensIf isComplex e1) (parensIf isComplex e2)
    Equals e1 e2 _  -> printf "%s == %s" (parensIf isComplex e1) (parensIf isComplex e2)

    Yes _           -> "true"
    No _            -> "false"

    Hole ty         -> printf "_:%s" (show ty)
    Plug e1 e2 _    -> printf "%s[%s]" (parensIf isComplex e2) (show e1)

    where
      parensIf :: (Expr -> Bool) -> Expr -> String
      parensIf cond e1
        | cond e1   = printf "(%s)" (show e1)
        | otherwise = show e1
