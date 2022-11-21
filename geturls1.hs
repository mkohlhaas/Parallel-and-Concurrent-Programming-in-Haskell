import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.ByteString as B (length)
import GetURL (getURL)

main ∷ IO ()
main = do
  m1 ← newEmptyMVar
  m2 ← newEmptyMVar

  forkIO $ do
    r ← getURL "http://www.wikipedia.org/wiki/Shovel"
    putMVar m1 r

  forkIO $ do
    r ← getURL "http://www.wikipedia.org/wiki/Spade"
    putMVar m2 r

  r1 ← takeMVar m1
  r2 ← takeMVar m2
  print (B.length r1, B.length r2)
