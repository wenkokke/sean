{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module SeAn.Annotation.Parsing where

import SeAn.Lexicon
import SeAn.Annotation.Base

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Idioms
import Text.ParserCombinators.UU.Utils

parseAWord :: String -> Annotation
parseAWord = runParser "stdin" pAWord

parseATree :: String -> Tree Annotation
parseATree = runParser "stdin" pATree

pAWord :: Parser Annotation
pAWord = iI Annotation pWord '|' pAnno Ii         <?> "annoted word"
  where
  pWord = pSome (pLetter <|> pDigit <|> pSym '_') <?> "word"
  pAnno = pName                                   <?> "annotation"
  
pATree :: Parser (Tree Annotation)
pATree = pLeaf <|> pNode
  where
  pNode = pBrackets (iI Node pATree pATree Ii)    <?> "annotation tree"
  pLeaf = Leaf <$> pAWord
