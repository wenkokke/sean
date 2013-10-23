{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.Lexicon.Parsing where

import Prelude hiding (abs,not)

import SeAn.Lexicon.Base
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
pDecl = iI decl pName pNames '=' (lexeme pExpr) Ii      <?> "declaration"

pDecls :: Parser [Decl]
pDecls = pList1 (iI pDecl ';' Ii)

pExpr :: Parser Expr
pExpr = pLet <|> pGAbs <|> pBin
  where
  -- parse simple terms
  pCon  = iI Con pName ":" pShortType Ii                <?> "constructor"
  pVar  = iI Var pName Ii                               <?> "variable"
  pPos  = pCon <|> pVar <|> pParens pExpr
  pNeg  = iI not '~' pPos Ii                            <?> "negation"
  pAtom = pPos <|> pNeg                                 <?> "atomic expression"
  
  -- parse let-bindings
  pLet  = iI letn "let" pDecls "in" pExpr Ii            <?> "let-binding"
  
  -- parse abstractions
  pAbs  = iI abs  "\\" pNames1 "." pExpr Ii             <?> "lambda abstraction"
  pUniv = iI univ "!"  pNames1 "." pExpr Ii             <?> "universal abstraction"
  pExis = iI exis "?"  pNames1 "." pExpr Ii             <?> "existential abstraction"
  pIota = iI iota "i"  pNames1 "." pExpr Ii             <?> "iota abstraction"
  pGAbs = pAbs <|> pUniv <|> pExis <|> pIota            <?> "abstraction"
  
  -- parse applications
  pApp  = pChainl_ng (App <$ pSpaces) pAtom
  pBin  = foldr pChainl_ng pApp $ map samePrio bins
    where
    samePrio ops = foldr (<|>) empty [ p <$ pSymbol op | (op, p) <- ops ]
  
pType :: Parser Type
pType = pChainl (iI TyArr "->" Ii) pAtom                <?> "type"
  where
  pAtom = TyCon <$> pSymbol "e"
      <|> TyCon <$> pSymbol "t"
      <|> pParens pType
      
pShortType :: Parser Type
pShortType = foldr1 TyArr <$> pList1 pAtom              <?> "shorttype"
  where
  pAtom = TyCon <$> (:[]) <$> pAnySym "et"
      <|> pParens pShortType

pName :: Parser Name
pName = lexeme $ (:) <$> pLetter <*> pMany pAlphaNum   <?> "identifier"
  where
  pAlphaNum = pLetter <|> pDigit <|> pSym '_'

pNames :: Parser [Name]
pNames = pListSep pSpaces pName

pNames1 :: Parser [Name]
pNames1 = pList1Sep pSpaces pName
