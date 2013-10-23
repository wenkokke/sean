module SeAn.Lexicon.Ambiguous where

import SeAn.Lexicon.Base

import Data.Map (Map)
import qualified Data.Map as M

disambiguate :: Prog -> Prog
disambiguate = undefined

freq :: Eq a => [a] -> a -> Int
freq [    ] = \_ -> 0
freq (x:xs) = \y -> (if x == y then 1 else 0) + freq xs y

