{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}
module SeAn.Logic.Base where

import Prelude hiding (pred)
import Text.Printf (printf)
import Data.String (IsString (..))
import Data.String.Utils (join)

data Nat where
  Nz :: Nat
  Ns :: Nat -> Nat

data Fin (n :: Nat) where
  Fz :: Fin (Ns n)
  Fs :: Fin n -> Fin (Ns n)

type family Max (m :: Nat) (n :: Nat) :: Nat
type instance Max  m      Nz    = m
type instance Max  Nz     n     = n
type instance Max (Ns m) (Ns n) = Ns (Max m n)

type family Ni (n :: Nat) :: Nat
type instance Ni  Nz    = Nz
type instance Ni (Ns n) = Ns (Ns n)


-- * datatype declarations

type Name (n :: Nat) = String
type Term = Logic Nz
type L0   = Logic (Ns Nz)
type L1   = Logic (Ns (Ns Nz))
type L2   = Logic (Ns (Ns (Ns Nz)))
type HOL  = forall n. Logic (Ns (Ns (Ns n)))

data Logic (n :: Nat) where
  Var    :: Name Nz -> Logic Nz
  Con    :: Name Nz -> Logic Nz
  Fun    :: Name Nz -> [Logic Nz] -> Logic Nz

  Pred   :: Name m -> [Logic n] -> Logic (Max m (Ni n))
  Exists :: Name m ->  Logic n  -> Logic (Max (Ns m) n)
  Forall :: Name m ->  Logic n  -> Logic (Max (Ns m) n)

  Top    :: Logic m
  Bot    :: Logic m
  Neg    :: Logic m -> Logic n
  Conj   :: Logic m -> Logic n -> Logic (Max m n)
  Disj   :: Logic m -> Logic n -> Logic (Max m n)
  Impl   :: Logic m -> Logic n -> Logic (Max m n)
  Equiv  :: Logic m -> Logic n -> Logic (Max m n)


-- * instance declarations

instance Show (Logic n) where
  show (Var x)       = x
  show (Con x)       = x
  show (Fun n xs)    = printf "%s(%s)" n (join ", " . map show $ xs)

  show (Pred n xs)   = printf "%s(%s)" n (join ", " . map show $ xs)
  show (Exists n xs) = printf "?%s.%s" n (show xs)
  show (Forall n xs) = printf "!%s.%s" n (show xs)

  show  Top          = "0"
  show  Bot          = "1"
  show (Neg x)       = printf "~(%s)" (show x)
  show (Conj x y)    = printf "%s /\\ %s" (show x) (show y)
  show (Disj x y)    = printf "%s \\/ %s" (show x) (show y)
  show (Impl x y)    = printf "%s => %s" (show x) (show y)
  show (Equiv x y)   = printf "%s == %s" (show x) (show y)


-- * examples of encoded logical formulas

prop :: Name (Ns Nz) -> L0
prop n = Pred n ([] :: [Term])

(-->) :: Logic m -> Logic n -> Logic (Max m n)
(-->) = Impl

example1 :: L0
example1 = prop "it_rains" --> prop "john_is_wet"
