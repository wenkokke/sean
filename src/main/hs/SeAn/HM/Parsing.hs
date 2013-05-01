{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.HM.Parsing where

import SeAn.HM.Base
import SeAn.Utils.Parsing
import qualified SeAn.HM.StdLib as Std
import Data.Maybe (catMaybes,maybeToList)
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Utils (pLetter,pLower,pUpper,pDigit)
import qualified Text.ParserCombinators.UU.Utils as UU (pSpaces,lexeme,pSymbol)

-- |Parses a lexicon in the extended syntax.
pLx :: Parser Lx
pLx = Lx_Lx <$ UU.pSpaces <*> pMany pSt

-- |Parses statements in the extended lexicon syntax.
pSt :: Parser St
pSt = UU.lexeme (pStTy <|> pStTm)
  where
  pStTy = St_Ty <$> pMbAn <*> pStId
  pStId = Ident_Ident <$> lexeme pVar <* pSymbol ":" <*> (Just <$> pTy)
  pStTm = St_Tm <$> pMbAn <*> lexeme pVar <* pSymbol "=" <*> pTm

-- |Parses an annotation.
pAn :: Parser An
pAn = pPubl <|> pPriv <|> pAuto
  where
  pPubl = An_Publ <$ UU.pSymbol "public"
  pPriv = An_Priv <$ UU.pSymbol "private"
  pAuto = An_Auto <$ UU.pSymbol "automatic"
  
-- |Parses an optional annotation.
pMbAn :: Parser MbAn
pMbAn = pMaybe pAn

-- |Parses types in the extended lexicon syntax.
pTy :: Parser Ty
pTy = pAll <|> pApp2
  where
  pAll  = uncurry (foldr Ty_All) <$ pSymbol "!" <*> pTyVars
  pApp2 = foldr1 Ty_App <$> pList1Sep_ng (pSymbol "->") (lexeme pApp1)
  pApp1 = foldr1 Ty_App <$> pList1Sep_ng pSucceed pAtom
  pAtom = Ty_Var <$> pTyVar <|> pParens pTy

-- |Parses a type variable in the extended lexicon syntax.
pTyVar :: Parser TyVar
pTyVar = pLower

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
  pOp f op e = foldl1 (Tm_App . Tm_App f) <$> pList1Sep op e
  pApp       = foldl1 Tm_App <$> pList1Sep pSpaces pAtom
  pAtom      = lexeme (pVar <|> pNeg)
  pNeg       = Tm_App Std.not <$ pSym '~' <*> pVar
  pVar       = Tm_Var <$> pIdent <|> pParens pTm

  -- |Parses quantifying terms.
  pForall    = quantify Std.forall <$ pSymbol "!"  <*> pIdents
  pExists    = quantify Std.exists <$ pSymbol "?"  <*> pIdents
  pIota      = quantify Std.iota   <$ pSymbol "i"  <*> pIdents
  pLambda    = uncurry (foldr Tm_Lam) <$ pSymbol "\\" <*> pIdents
  quantify f = uncurry (foldr $ (Tm_App f .) . Tm_Lam)

-- |Parses identifiers in the extended lexicon syntax.
pVar :: Parser Var
pVar = (:) <$> pLetter <*> pMany pAlphaNum_ <?> "Ident"

-- |Parses an optionally typed identifier in the extended lexicon syntax.
pIdent :: Parser Ident
pIdent = Ident_Ident <$> pVar <*> pMaybe (pSym ':' *> pTy)

-- |Parses a sequence of typed identifiers.
pIdents :: Parser (Tm,[Ident])
pIdents = flip (,) <$> pList1Sep pSpaces pIdent <* pDot <*> pTm
