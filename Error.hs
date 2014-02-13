module Error where

import Base (Name)
import Control.Monad.Error (ErrorT,runErrorT)
import Control.Monad.Supply (Supply,evalSupply)

type Message = String
type Error a = ErrorT Message (Supply Name) a

isError :: Error a -> Bool
isError = either (const True) (const False) . supplyFreshNames

supplyFreshNames :: Error a -> Either String a
supplyFreshNames e = evalSupply (runErrorT e) freshNames

freshNames :: [Name]
freshNames = letters ++ numbers
  where
  letters    = fmap (: []) (['A'..'D'] ++ ['F'..'S'] ++ ['U'..'Z'])
  numbers    = fmap (('T' :) . show) ([0 ..] :: [Int])
