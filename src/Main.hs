import Sean
import Control.Monad (forM_)
import Control.Applicative ((<$>))
import Text.Printf (printf)

main :: IO ()
main = do
  result <- evalProg <$> getContents
  case result of
    Right bool -> print bool
    Left missing -> do
         forM_ missing $ \(name , tyAnn) ->
               putStrLn $ printf "%s : %s" name (maybe "?" show tyAnn)
