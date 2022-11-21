import Async (concurrently)
import qualified Data.ByteString as B
import GetURL (getURL)

sites ∷ [String]
sites =
  [ "http://www.google.com",
    "http://www.bing.com",
    "http://www.yahoo.com",
    "http://www.wikipedia.com/wiki/Spade",
    "http://www.wikipedia.com/wiki/Shovel"
  ]

main ∷ IO ()
main = do
  xs ← foldr conc (return []) (map getURL sites)
  print (map B.length xs)
  where
    conc ioa ioas = do
      (a, as) ← concurrently ioa ioas
      return (a : as)
