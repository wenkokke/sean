import SeAn.HM.Base
import SeAn.HM.StdLib
import SeAn.HM.Parsing
import SeAn.HM.Printing

import Data.String.Utils
import Text.ParserCombinators.UU (amb,(<$>),(<*>))
import Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = getContents >>= print . parse "stdin"

-- |Parses a lexicon file without birdtags.
lx = runParser "stdin" pLx

-- |Parses a lexicon file with birdtags.
parse file = runParser file pLx . birdtags

-- |Removes all lines without birdtags.
birdtags :: String -> String
birdtags = join "\n" . map (rstrip . stripbirdtag) . filter hasbirdtag . lines
  where
  hasbirdtag   = startswith "> "
  stripbirdtag = drop 2