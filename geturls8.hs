import Async (concurrently)
import qualified Data.ByteString as B
import GetURL (getURL)

main ∷ IO ()
main = do
  (r1, r2) <-
    concurrently
      (getURL "http://www.wikipedia.org/wiki/Shovel")
      (getURL "http://www.wikipedia.org/wiki/Spade")
  print (B.length r1, B.length r2)
