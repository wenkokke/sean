{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.Lexicon.HM.Parsing where

import Data.Maybe (catMaybes,maybeToList)

import SeAn.Utils.Parsing
import SeAn.Lexicon.HM.Base as Std

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Utils (pLetter,pLower,pUpper,pDigit)
import qualified Text.ParserCombinators.UU.Utils as UU (pSpaces,lexeme,pSymbol)

-- |Parses a lexicon in the extended syntax.
pLexicon :: Parser Lexicon
pLexicon = Lexicon <$ UU.pSpaces <*> pMany pDecl

-- |Parses statements in the extended lexicon syntax.
pDecl :: Parser Decl
pDecl = UU.lexeme (pTyDecl <|> pTmDecl)
  where
  pTyDecl = TyDecl <$> pMbAn <*> pIdent'
  pIdent' = Ident  <$> lexeme pVar <* pSymbol ":" <*> (Just <$> pTy)
  pTmDecl = TmDecl <$> pMbAn <*> lexeme pVar <* pSymbol "=" <*> pTm

-- |Parses an annotation.
pAn :: Parser An
pAn = pPubl <|> pPriv
  where
  pPubl = Publ <$ UU.pSymbol "public"
  pPriv = Priv <$ UU.pSymbol "private"
  
-- |Parses an optional annotation.
pMbAn :: Parser MbAn
pMbAn = pMaybe pAn

-- |Parses types in the extended lexicon syntax.
pTy :: Parser Ty
pTy = pAll <|> pApp2
  where
  pAll  = uncurry (foldr TyAll) <$ pSymbol "!" <*> pTyVars
  pApp2 = foldr1 TyApp <$> pList1Sep_ng (pSymbol "->") (lexeme pApp1)
  pApp1 = foldr1 TyApp <$> pList1Sep_ng pSucceed pAtom
  pAtom = TyVar <$> pTyVar <|> pParens pTy

-- |Parses a type variable in the extended lexicon syntax.
pTyVar :: Parser TyVar
pTyVar = pure <$> pLower

-- |Parses type variables in the extended lexicon syntax.
pTyVars :: Parser (Ty,[TyVar])
pTyVars = flip (,) <$> pList1Sep pSpaces pTyVar <* pDot <*> pTy

-- |Parses terms in the extended lexicon syntax.
pTm :: Parser Tm
pTm = pLambda <|> pForall <|> pExists <|> pIota <|> pTerm
  where

  -- |Parses simple terms.
  pTerm      = pEquiv
  pEquiv     = pOp Std.eq      (pSymbol "==")  pImpl
  pImpl      = pOp Std.implies (pSymbol "=>")  pDisj
  pDisj      = pOp Std.or      (pSymbol "\\/") pConj
  pConj      = pOp Std.and (pSymbol "/\\") (lexeme pApp)
  pOp f op e = foldl1 (App . App f) <$> pList1Sep op e
  pApp       = foldl1 App <$> pList1Sep pSpaces pAtom
  pAtom      = lexeme (pVar <|> pNeg)
  pNeg       = App Std.not <$ pSym '~' <*> pVar
  pVar       = Var <$> pIdent <|> pParens pTm

  -- |Parses quantifying terms.
  pForall    = quantify Std.forall <$ pSymbol "!"  <*> pIdents
  pExists    = quantify Std.exists <$ pSymbol "?"  <*> pIdents
  pIota      = quantify Std.iota   <$ pSymbol "i"  <*> pIdents
  pLambda    = uncurry (foldr Lam) <$ pSymbol "\\" <*> pIdents
  quantify f = uncurry (foldr $ (App f .) . Lam)

-- |Parses identifiers in the extended lexicon syntax.
pVar :: Parser Var
pVar = (:) <$> pLetter <*> pMany pAlphaNum_ <?> "Ident"

-- |Parses an optionally typed identifier in the extended lexicon syntax.
pIdent :: Parser Ident
pIdent = Ident <$> pVar <*> pMaybe (pSym ':' *> pTy)

-- |Parses a sequence of typed identifiers.
pIdents :: Parser (Tm,[Ident])
pIdents = flip (,) <$> pList1Sep pSpaces pIdent <* pDot <*> pTm
