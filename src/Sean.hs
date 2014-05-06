module Sean where

import Base
import Error
import Parsing
import Resolving
import Reducing
import qualified Data.Map as M (lookup)
import qualified Data.Maybe as M (fromJust)
import qualified Data.List as L (nub)
import Data.Foldable (forM_)
import Control.Applicative ((<$>))
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (runParser,pNatural)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.Printf (printf)


evalProg :: String -> Either [(Name,TyAnn)] Bool
evalProg contents = case toBool text of
         Just b  -> Right b
         Nothing -> Left $ L.nub $ filter (not . isPrim) (freeIdents text)
  where
  file   = filter (not . null) . lines $ contents
  domain = runParser (head file) pDomainSize (head file)
  result = runPipeline domain (tail file)
  env    = fst . fromRight $ result
  text   = M.fromJust . M.lookup "text" $ env

runPipeline :: Size -> [String] -> Either String (Env , TyEnv)
runPipeline size ds0 = supplyFreshNames ds4
  where
    ds1 :: [String]
    ds1 = filter (not . null) ds0
    ds2 :: [Decl]
    ds2 = map (runParser "stdin" pDecl) ds1
    ds3 :: Error [Decl]
    ds3 = disambiguate ds2
    ds4 :: Error (Env , TyEnv)
    ds4 = ds3 >>= eval size


fromRight :: Either a b -> b
fromRight (Right x) = x
