module SeAn.Annotation.Base where

import SeAn.Lexicon (Name)

import Text.Printf (printf)

data Annotation
   = Annotation { text :: String , category :: Name }
   deriving Eq
  
data Tree a
   = Leaf a
   | Node (Tree a) (Tree a)
   deriving Eq

instance Show Annotation where
  show (Annotation str cat) = printf "%s|%s" str cat
  
instance (Show a) => Show (Tree a) where
  show (Leaf x)   = show x
  show (Node l r) = printf "[%s %s]" (show l) (show r)
