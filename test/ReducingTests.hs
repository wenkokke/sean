import Sean (evalProg)
import Paths_sean (getDataFileName)
import System.Exit (exitSuccess,exitFailure)
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (runParser,pNatural)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))

main :: IO ()
main = do
     examplePath <- getDataFileName "examples/example.sean"
     example <- readFile examplePath
     case evalProg example of
          Right bool -> if bool then exitSuccess else exitFailure
          Left missing -> exitFailure
