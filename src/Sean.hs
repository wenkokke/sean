module Sean where

import Base
import Error
import Parsing
import Resolving
import Reducing
import qualified Data.Map as M (lookup)
import qualified Data.Maybe as M (fromJust)
import qualified Data.List as L (nub,find)
import Data.Foldable (forM_)
import Control.Applicative ((<$>))
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (runParser,pNatural)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.Printf (printf)


findDecl :: Name -> Prog -> Maybe Decl
findDecl n1 (Prog _ ds _) = L.find (\(Decl n2 _ _) -> n1 == n2) ds

getExpr :: Decl -> Expr
getExpr (Decl _ _ e) = e

runProg :: String -> Either [(Name,TyAnn)] Bool
runProg contents = case toBool text of
  Just b  -> Right b
  Nothing -> Left $ L.nub $ filter (not . isPrim) (freeIdents text)
  where
  env    = fromRight . evalProg . parseProg $ contents
  text   = getExpr . M.fromJust . findDecl "text" $ env

fromRight :: Either a b -> b
fromRight (Right x) = x
