{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Parsing where

import Prelude hiding (abs,fst,snd)
import Base
import Control.Applicative ((<$),(<$>),(<|>),(<*),(<*>),(*>))
import Data.Foldable (asum)
import Data.Either (partitionEithers)
import Text.ParserCombinators.UU
  ((<?>),(<??>),pList,pList1,pList1Sep,pMaybe,pChainl,pEither,empty)
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils
  (runParser,lexeme,pAnySym,pParens,pBraces,pLetter
     ,pTuple,pDigit,pComma,pSpaces,pNatural,pSymbol)



-- * Global constants

holeName :: Name
holeName = "_"


-- * Parsing

-- ** Parsing programs

parseProg :: String -> Prog
parseProg contents = uncurry (Prog domainSize) (partitionEithers eithers)
  where
     file       = filter ((/="--") . take 2) . filter (not . null) . lines $ contents
     domainSize = runParser "parseProg" pDomainSize (head file)
     eithers    = map (runParser "parseProg" (pEither pDecl pRewr)) (tail file)

-- ** Parsing the domain size

pDomainSize :: Parser Int
pDomainSize = lexeme $ iI "domain_size" '=' pNatural Ii

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

pTup2 :: Parser (Expr,Expr)
pTup2 = pParens $ (,) <$> pExpr <* pComma <*> pExpr
pTup3 :: Parser (Expr,Expr,Expr)
pTup3 = pParens $ (,,) <$> pExpr <* pComma <*> pExpr <* pComma <*> pExpr

pExpr,pVar,pObj,pRel1,pRel2,pRel3,pHole,pPlug :: Parser Expr
pExpr = pBin
pVar  = iI Var pName (pMaybe pTyAnn) Ii <?> "Variable"
pObj  = iI Obj pNatural (pMaybe pTyAnn) Ii <?> "Object"
pRel1 = Rel1 <$> pBraces (pList1Sep pComma pExpr) <*> pMaybe pTyAnn <?> "Rel1"
pRel2 = Rel2 <$> pBraces (pList1Sep pComma pTup2) <*> pMaybe pTyAnn <?> "Rel2"
pRel3 = Rel3 <$> pBraces (pList1Sep pComma pTup3) <*> pMaybe pTyAnn <?> "Rel3"
pHole = iI Hole holeName (pMaybe pTyAnn) Ii <?> "Hole"
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
pExprAtom = pVar  <|> pGGQ
        <|> pObj  <|> pRel1 <|> pRel2 <|> pRel3
        <|> pPlug <|> pHole
        <|> pParens pExpr <??> pOptTyAnn

pApp :: Parser Expr
pApp = pChainl (app <$ pSpaces) pExprAtom

pBin :: Parser Expr
pBin = foldr (pChainl . samePrio) pApp bins
  where
    samePrio :: [(String, a)] -> Parser a
    samePrio ops = asum [ p <$ pSymbol op | (op, p) <- ops ]

bins :: [[(String , Expr -> Expr -> Expr)]]
bins =
  [ [ ("=="  , fun2 "EQUALS") ]
  , [ ("\\/" , fun2 "OR") , ("/\\" , fun2 "AND") ]
  , [ ("=>"  , fun2 "IMPLIES") , ("<=" , flip (fun2 "IMPLIES")) ]
  , [ ("<=>" , fun2 "EQUIV") ]
  ]


-- ** Parsing declarations

pDecl :: Parser Decl
pDecl = iI Decl pName (pMaybe pTyAnn) '=' pExpr Ii


-- ** Parsing rules

pRewr :: Parser Rewr
pRewr = iI Rewr pExpr "->" pExpr Ii
