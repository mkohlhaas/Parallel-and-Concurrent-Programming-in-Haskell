-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.
--
-- Sample geturls.hs (CEFP summer school notes, 2011)
--
-- Downloading multiple URLs concurrently, timing the downloads
--
-- Compile with:
--    ghc -threaded --make geturls.hs

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import GetURL
import Text.Printf
import TimeIt

data Async a = Async (MVar a)

async ∷ IO a → IO (Async a)
async action = do
  var ← newEmptyMVar
  forkIO (action >>= putMVar var)
  return (Async var)

wait ∷ Async a → IO a
wait (Async var) = readMVar var

sites =
  [ "http://www.google.com",
    "http://www.bing.com",
    "http://www.yahoo.com",
    "http://www.wikipedia.com/wiki/Spade",
    "http://www.wikipedia.com/wiki/Shovel"
  ]

main = mapM (async . http) sites >>= mapM wait
  where
    http url = do
      (page, time) ← timeit $ getURL url
      printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time
