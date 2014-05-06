import Parsing (pDecl,pDomainSize)
import Paths_sean (getDataFileName)
import System.Exit (exitSuccess,exitFailure)
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (runParser,pNatural)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))

main :: IO ()
main = do
     examplePath <- getDataFileName "examples/example.sean"
     example <- readFile examplePath
     let file   = filter (not . null) (lines example)
     let domain = runParser (head file) pDomainSize (head file)
     let decls  = map (runParser "stdin" pDecl) (tail file)
     domain `seq` decls `seq` exitSuccess
