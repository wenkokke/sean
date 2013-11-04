{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.Lexicon.Parsing where

import Prelude hiding (abs,not)

import SeAn.Lexicon.Base
import Data.Char (toUpper)

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromJust)

import Text.ParserCombinators.UU ((<$),(<$>),(<*>),(*>),(<|>),(<?>)
  ,empty,pMany,pList1,pListSep,pList1Sep,pChainl,pChainl_ng)
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils (runParser,pSpaces,lexeme
  ,pSymbol,pParens,pLetter,pDigit,pAnySym)

parseProg :: String -> Prog Name
parseProg = runParser "stdin" pProg

parseDecl :: String -> Decl Name
parseDecl = runParser "stdin" pDecl

parseExpr :: String -> Expr Name
parseExpr = runParser "stdin" pExpr

parseType :: String -> Type
parseType = runParser "stdin" pType

parseShortType :: String -> Type
parseShortType = runParser "stdin" pShortType

-- |Decide whether something is a bound or a free variable, and change
--  the constructor accordingly. Separated to avoid monadic parsers.
setConOrVar :: Eq n => [n] -> Expr n -> Expr n
setConOrVar ns exp = let

  rewrap n = if n `elem` ns then Var n else Con n

  in case exp of
  Con  n       -> rewrap n
  Var  n       -> rewrap n
  Abs  n e     -> Abs n (setConOrVar (n:ns) e)
  App    e1 e2 -> App (setConOrVar ns e1) (setConOrVar ns e2)
  Let  n e1 e2 -> Let n (setConOrVar ns e1) (setConOrVar (n:ns) e2)
  Hole t       -> Hole t
  Inst n a     -> Inst n a


pProg :: Parser (Prog Name)
pProg = pSpaces *> iI Prog pDecls Ii

pDecl :: Parser (Decl Name)
pDecl = iI decl pName pNames '=' (lexeme (setConOrVar [] <$> pExpr)) Ii <?> "declaration"

pDecls :: Parser [Decl Name]
pDecls = pList1 (iI pDecl ';' Ii)

pExpr :: Parser (Expr Name)
pExpr = pLet <|> pGAbs <|> pBin
  where
  -- parse simple terms
  pHole = iI Hole '_' ':' pShortType Ii                 <?> "hole"
  pVar  = iI Var pName Ii                               <?> "variable"
  pInst = iI Inst pName '@' pName Ii                    <?> "hole instantiation"
  pPos  = pHole <|> pInst <|> pVar <|> pParens pExpr
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
