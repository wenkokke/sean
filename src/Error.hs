module Error where

import Base (Name)
import Control.Monad.Trans.Except (ExceptT,runExceptT)
import Control.Monad.Supply (Supply,evalSupply)

type Message = String
type Error a = ExceptT Message (Supply Name) a

isError :: Error a -> Bool
isError = either (const True) (const False) . supplyFreshNames

supplyFreshNames :: Error a -> Either String a
supplyFreshNames e = evalSupply (runExceptT e) freshNames

freshNames :: [Name]
freshNames = letters ++ numbers
  where
  letters    = fmap (: []) (['A'..'D'] ++ ['F'..'S'] ++ ['U'..'Z'])
  numbers    = fmap (('T' :) . show) ([0 ..] :: [Int])
