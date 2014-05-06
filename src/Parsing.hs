{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parsing where

import Prelude hiding (abs,fst,snd)
import Base
import Control.Applicative ((<$),(<$>),(<|>),(<*>))
import Data.Foldable (asum)
import Text.ParserCombinators.UU
  ((<?>),(<??>),pList,pList1,pList1Sep,pMaybe,pChainl,empty)
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils
  (runParser,lexeme,pAnySym,pParens,pBraces,pLetter
            ,pDigit,pComma,pSpaces,pNatural,pSymbol)



-- * Global constants

holeName :: Name
holeName = "_"


-- * Parsing

-- ** Parsing the domain size

pDomainSize :: Parser Int
pDomainSize = iI "domain_size" '=' pNatural Ii

-- ** Parsing types and type annotations

pType :: Parser Type
pType = foldr1 TyArr <$> pList1 pAtom <?> "Type"
  where
    pAtom = TyCon . return <$> pAnySym "et"
        <|> pParens pType

pTyAnn :: Parser Type
pTyAnn = iI ':' pType Ii <?> "TypeAnnotation"

pOptTyAnn :: HasType a => Parser (a -> a)
pOptTyAnn = flip annotate . Just <$> pTyAnn


-- ** Parsing names

pName :: Parser Name
pName = lexeme (iI (:) pLetter (pList pAlphaNum_) Ii) <?> "Name"

pNames :: Parser [Name]
pNames = pList1Sep pSpaces pName

pAlphaNum_ :: Parser Char
pAlphaNum_ = pLetter <|> pDigit <|> pSym '_'


-- ** Parsing expressions

pExpr,pVar,pPair,pCase,pObj,pSet :: Parser Expr
pExpr = pBin
pVar  = iI Var pName (pMaybe pTyAnn) Ii <?> "Variable"
pPair = iI Pair '(' pExpr ',' pExpr ')' (pMaybe pTyAnn) Ii <?> "Tuple"
pCase = iI Case '\\' '(' pName ',' pName ')' '.' pExpr (pMaybe pTyAnn) Ii <?> "Case"
pObj  = iI Obj pNatural (pMaybe pTyAnn) Ii <?> "Object"
pSet  = Set <$> pBraces (pList1Sep pComma pExpr) <*> pMaybe pTyAnn <?> "Set"


pPlug :: Parser Expr
pPlug = iI plug pName '[' pName (pMaybe pTyAnn) ']' Ii <?> "Plug"
  where
    plug :: Name -> Name -> TyAnn -> Expr
    plug c e t = Plug (Var e t) (var c) Nothing


pGGQ :: Parser Expr
pGGQ  = pAbs <|> pUniv <|> pExis <|> pIota
  where
    pAbs  = iI absn '\\' pNames '.' pExpr Ii <?> "Lambda"
    pUniv = iI univ '!'  pNames '.' pExpr Ii <?> "Universal"
    pExis = iI exis '?'  pNames '.' pExpr Ii <?> "Existential"
    pIota = iI iota 'i'  pNames '.' pExpr Ii <?> "Iota"

pExprAtom :: Parser Expr
pExprAtom = pVar <|> pGGQ
          <|> pObj <|> pSet
          <|> pPair <|> pCase
          <|> pPlug <|> pHole <|>
              (pParens pExpr <??> pOptTyAnn)

pApp :: Parser Expr
pApp = pChainl (app <$ pSpaces) pExprAtom

pBin :: Parser Expr
pBin = foldr (pChainl . samePrio) pApp bins
  where
    samePrio :: [(String, a)] -> Parser a
    samePrio ops = asum [ p <$ pSymbol op | (op, p) <- ops ]

pHole :: Parser Expr
pHole = iI Hole holeName (pMaybe pTyAnn) Ii <?> "Hole"


-- ** Parsing declarations

pDecl :: Parser Decl
pDecl = iI Decl pName (pMaybe pTyAnn) '=' pExpr Ii
