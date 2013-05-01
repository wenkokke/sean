{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.Utils.Parsing where

import Data.ListLike (ListLike)
import Text.ParserCombinators.UU (P,IsParser,list_alg,IsLocationUpdatedBy,pure,pFoldr1,pFoldr1_ng,must_be_non_empties,(<*),(<*>),(*>),(<$>),(<?>),(<??>),(<<|>))
import Text.ParserCombinators.UU.BasicInstances (Parser,ParserTrafo,Str,pSym,pMunch,pToken)
import Text.ParserCombinators.UU.Utils (pLetter,pDigit,pAnySym)

pFoldr2Sep    ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr2Sep     alg@(op,e) sep p =  must_be_non_empties "pFoldr2Sep"    sep   p pfm
                                   where pfm = op <$> p <*> pFoldr1    alg (sep *> p)
pFoldr2Sep_ng ::   IsParser p => (a -> b -> b, b) -> p a1 ->p a -> p b
pFoldr2Sep_ng  alg@(op,e) sep p =  must_be_non_empties "pFoldr2Sep_ng" sep   p pfm 
                                   where pfm = op <$> p <*> pFoldr1_ng alg (sep *> p)

pList2Sep    :: IsParser p => p a1 -> p a -> p [a]
pList2Sep     s p =  must_be_non_empties "pListSep"    s   p (pFoldr2Sep    list_alg s p)

pList2Sep_ng :: IsParser p => p a1 -> p a -> p [a]
pList2Sep_ng  s p =  must_be_non_empties "pListSep_ng" s   p (pFoldr2Sep_ng list_alg s p)

-- |Parses a single space (excluding newlines).
pSpace :: Parser Char
pSpace = pAnySym " \t" <?> "Whitespace"

-- |Parses many spaces (excluding newlines).
pSpaces :: Parser String
pSpaces = pMunch (`elem` " \r\t") <?> "Whitespace"

-- |Parses some spaces (excluding newlines).
pSpaces1 :: Parser String
pSpaces1 = (:) <$> pSpace <*> pMunch (`elem` " \r\t") <?> "Whitespace"

-- |Lexeme Parsers skip trailing whitespace (excluding newlines).
lexeme :: ParserTrafo a a
lexeme p = p <* pSpaces

-- |Creates a parser optionally followed by a single space.
optspace :: ParserTrafo a a
optspace p = p <??> (flip const <$> pSpace)

-- |Creates a lexeme parser for a symbol.
pSymbol :: (IsLocationUpdatedBy loc Char, ListLike state Char) => String -> P (Str Char state loc) String
pSymbol str = lexeme (pToken str)

-- |Empty parser. i.e. the parser that always succeeds.
pSucceed :: Parser ()
pSucceed = pure ()

-- * Parsers for various character classes
pAlphaNum :: Parser Char
pAlphaNum = pLetter <<|> pDigit

pAlphaNum_ :: Parser Char
pAlphaNum_ = pAlphaNum <<|> pSym '_'

-- * Parsers for various symbols
pDot, pComma, pDQuote, pLParen, pRParen, pLBracket, pRBracket, pLBrace, pRBrace :: Parser Char
pDot      = lexeme $ pSym '.'
pComma    = lexeme $ pSym ','
pDQuote   = lexeme $ pSym '"'
pLParen   = lexeme $ pSym '('
pRParen   = lexeme $ pSym ')'
pLBracket = lexeme $ pSym '['
pRBracket = lexeme $ pSym ']'
pLBrace   = lexeme $ pSym '{'
pRBrace   = lexeme $ pSym '}'

-- * Parenthesized structures
pParens :: ParserTrafo a a
pParens p = pLParen *> p <* pRParen

pBraces ::  ParserTrafo a a
pBraces p = pLBrace *> p <* pRBrace

pBrackets ::  ParserTrafo a a
pBrackets p = pLBracket *> p <* pRBracket