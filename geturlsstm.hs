-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.
--
-- Sample geturls.hs (CEFP summer school notes, 2011)
--
-- Downloading multiple URLs concurrently, timing the downloads
--
-- Compile with:
--    ghc -threaded --make geturls.hs

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, retry, writeTVar)
import Control.Exception ()
import Control.Monad ()
import qualified Data.ByteString as B
import GetURL (getURL)
import Text.Printf (printf)
import TimeIt (timeit)

data Async a = Async (TVar (Maybe a))

async ∷ IO a → IO (Async a)
async action = do
  var ← atomically $ newTVar Nothing
  forkIO (do a ← action; atomically (writeTVar var (Just a)))
  return (Async var)

wait ∷ Async a → IO a
wait (Async var) = atomically $ do
  m ← readTVar var
  case m of
    Nothing → retry
    Just a → return a

sites ∷ [String]
sites =
  [ "http://www.google.com",
    "http://www.bing.com",
    "http://www.yahoo.com",
    "http://www.wikipedia.com/wiki/Spade",
    "http://www.wikipedia.com/wiki/Shovel"
  ]

main ∷ IO [()]
main = mapM (async . http) sites >>= mapM wait
  where
    http url = do
      (page, time) ← timeit $ getURL url
      printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time
