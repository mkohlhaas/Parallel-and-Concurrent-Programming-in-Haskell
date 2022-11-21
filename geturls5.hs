import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM_)
import qualified Data.ByteString as B
import GetURL (getURL)
import Text.Printf (printf)

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
  m ← newEmptyMVar
  let download url = do
        r ← getURL url
        putMVar m (url, r)
  mapM_ (forkIO . download) sites
  (url, r) ← takeMVar m
  printf "%s was first (%d bytes)\n" url (B.length r)
  replicateM_ (length sites - 1) (takeMVar m)
