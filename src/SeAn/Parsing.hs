{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.Parsing where

import Prelude hiding (abs,not)

import SeAn.Base
import Data.Char (toUpper)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromJust)

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.Utils
    
parseProg :: String -> Prog
parseProg = runParser "stdin" pProg

parseDecl :: String -> Decl
parseDecl = runParser "stdin" pDecl

parseExpr :: String -> Expr
parseExpr = runParser "stdin" pExpr
    
parseType :: String -> Type
parseType = runParser "stdin" pType

parseShortType :: String -> Type
parseShortType = runParser "stdin" pShortType

pProg :: Parser Prog
pProg = pSpaces *> iI Prog pDecls Ii

pDecl :: Parser Decl
pDecl = iI decl pName pNames "=" pExpr Ii

pDecls :: Parser [Decl]
pDecls = pList1Sep (pSymbol ";") pDecl

pExpr :: Parser Expr
pExpr = pLet <|> pAbss <|> pBin
  where
  -- parse simple terms
  pCon  = iI Con pName ":" pShortType Ii
  pVar  = iI Var pName Ii <|> pParens pExpr
  pNot  = iI not "~" (pCon <|> pVar) Ii
  pAtom = pCon <|> pVar <|> pNot
  
  -- parse let-bindings
  pLet  = iI letn "let" pDecls "in" pExpr Ii
  
  -- parse abstractions
  pAbs  = iI abs  "\\" pNames1 "." pExpr Ii
  pUniv = iI univ "!" pNames1 "." pExpr Ii
  pExis = iI exis "?" pNames1 "." pExpr Ii
  pIota = iI iota "i" pNames1 "." pExpr Ii
  pAbss = pAbs <|> pUniv <|> pExis <|> pIota
  
  -- parse applications
  pApp  = pChainl_ng (App <$ pSpaces) pAtom
  pBin  = foldr pChainl pApp $ map samePrio bins
    where
    samePrio ops = foldr (<|>) empty [ p <$ pSymbol op | (op, p) <- ops ]
  
pType :: Parser Type
pType = pChainl (iI TyArr "->" Ii) pAtom
  where
  pAtom = TyCon <$> pSymbol "E"
      <|> TyCon <$> pSymbol "T"
      <|> pParens pType
      
pShortType :: Parser Type
pShortType = foldr1 TyArr <$> pList1 pAtom
  where
  pAtom = TyCon <$> (:[]) <$> toUpper <$> pAnySym "et"
      <|> pParens pShortType

pName :: Parser Name
pName = lexeme $ (++) <$> pSome (pLetter <|> pSym '_') <*> pMany pDigit

pNames :: Parser [Name]
pNames = pListSep pSpaces pName

pNames1 :: Parser [Name]
pNames1 = pList1Sep pSpaces pName
