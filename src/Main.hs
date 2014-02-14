{-# LANGUAGE FlexibleContexts #-}

module Main where

import Base
import Error
import Parsing
import Resolving
import Reducing
import qualified Data.Either.Utils as E (fromRight)
import qualified Data.Map as M (lookup)
import qualified Data.Maybe as M (fromJust)
import qualified Data.List as L (nub)
import Data.Foldable (forM_)
import Control.Applicative ((<$>))
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (runParser,pNatural)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.Printf (printf)

pDomainSize :: Parser Int
pDomainSize = iI "domain_size" '=' pNatural Ii

main :: IO ()
main = do
  file <- filter (not . null) . lines <$> getContents
  let domain = runParser (head file) pDomainSize (head file)
  let result = run domain (tail file)
  let env    = fst . E.fromRight $ result
  let text   = M.fromJust . M.lookup "text" $ env
  case toBool text of
    Just b  -> print b
    Nothing -> do
      let idents = L.nub $ filter (not . isPrim) (freeIdents text)
      forM_ idents $ \(name , tyAnn) ->
        putStrLn $ printf "%s : %s" name (maybe "?" show tyAnn)

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
