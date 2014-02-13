module Main where

import Base
import Error
import Parsing
import Resolving
import Typing
import Unification
import Substitution
import Reducing
import Text.ParserCombinators.UU.Utils (runParser)

run :: Size -> [String] -> Either String (Env , TyEnv)
run size ds0 = supplyFreshNames ds4
  where
    ds1 :: [String]
    ds1 = filter (not . null) ds0
    ds2 :: [Decl]
    ds2 = map (runParser "stdin" pDecl) ds1
    ds3 :: Error [Decl]
    ds3 = disambiguate ds2
    ds4 :: Error (Env , TyEnv)
    ds4 = ds3 >>= eval size
