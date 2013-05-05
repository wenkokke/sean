import SeAn.Lexicon.HM.Base
import SeAn.Lexicon.HM.Parsing
import SeAn.Lexicon.HM.Printing

import Data.String.Utils
import Text.ParserCombinators.UU (amb,(<$>),(<*>))
import Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = getContents >>= sequence_ . map (print . stl) . parse "stdin"

-- |Parses a lexicon file with birdtags.
parse file = runParser file (amb pLexicon) . birdtags

-- |Removes all lines without birdtags.
birdtags :: String -> String
birdtags = join "\n" . map (rstrip . stripbirdtag) . filter hasbirdtag . lines
  where
  hasbirdtag   = startswith "> "
  stripbirdtag = drop 2