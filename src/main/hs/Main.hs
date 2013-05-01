import SeAn.Lexicon.Base
import SeAn.Lexicon.StdLib
import SeAn.Lexicon.Parsing
import SeAn.Lexicon.Printing

import Data.String.Utils
import Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = getContents >>= print . runParser "StdIn" pLx

ty :: String -> Ty
ty = runParser "stdin" pTy

tm :: String -> Tm
tm = runParser "stdin" pTm