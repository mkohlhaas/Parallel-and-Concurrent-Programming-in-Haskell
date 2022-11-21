import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar)
import qualified Data.ByteString as B
import GetURL (getURL)

data Async a = Async (MVar a)

async ∷ IO a → IO (Async a)
async action = do
  var ← newEmptyMVar
  forkIO (do r ← action; putMVar var r)
  return (Async var)

wait ∷ Async a → IO a
wait (Async var) = readMVar var

main ∷ IO ()
main = do
  a1 ← async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 ← async (getURL "http://www.wikipedia.org/wiki/Spade")
  r1 ← wait a1
  r2 ← wait a2
  print (B.length r1, B.length r2)

-- >>
