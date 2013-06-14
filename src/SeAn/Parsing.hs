{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.Parsing where

import Prelude hiding (abs,not)

import SeAn.Base

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromJust)

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.Utils
    
parseType :: String -> Type
parseType = runParser "stdin" pType

parseExpr :: String -> Expr
parseExpr = runParser "stdin" pExpr

pName :: Parser Name
pName = lexeme $ (++) <$> pSome pLetter <*> pMany pDigit

pNames :: Parser [Name]
pNames = pList1Sep pSpaces pName

pExpr :: Parser Expr
pExpr = pLet <|> pAbss <|> pBin
  where
  -- parse simple terms
  pVar  = Var <$> pName <|> pParens pExpr
  pNeg  = iI not "~" pVar Ii
  pAtom = pVar <|> pNeg
  
  -- parse let-bindings
  pDef  = iI def pName "=" pExpr Ii
  pDefs = pList1Sep (pSymbol ";") pDef
  pLet  = iI letn "let" pDefs "in" pExpr Ii
  
  -- parse abstractions
  pAbs  = iI abs "\\" pNames "." pExpr Ii
  pUniv = iI univ "!" pNames "." pExpr Ii
  pExis = iI exis "?" pNames "." pExpr Ii
  pIota = iI iota "i" pNames "." pExpr Ii
  pAbss = pAbs <|> pUniv <|> pExis <|> pIota
  
  -- parse applications
  pApp  = pChainl_ng (App <$ pSpaces) pAtom
  pBin  = foldr pChainl pApp $ map samePrio bins
    where
    samePrio ops = foldr (<|>) empty [ p <$ pSymbol op | (op, p) <- ops ]
  
pType :: Parser Type
pType = pChainl (iI TyArr "->" Ii) pAtom
  where
  pAtom :: Parser Type
  pAtom = TyCon <$> pSymbol "E"
      <|> TyCon <$> pSymbol "T"
      <|> pParens pType
