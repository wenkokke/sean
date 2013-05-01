import Data.String.Utils

main :: IO ()
main = getContents >>= putStr . birdtags

-- |Removes all lines without birdtags.
birdtags :: String -> String
birdtags = join "\n" . map (rstrip . stripbirdtag) . filter hasbirdtag . lines
  where
  hasbirdtag   = startswith "> "
  stripbirdtag = drop 2