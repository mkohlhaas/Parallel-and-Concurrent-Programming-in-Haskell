-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.
--
-- Sample geturls.hs (CEFP summer school notes, 2011)
--
-- Downloading multiple URLs concurrently, timing the downloads
--
-- Compile with:
--    ghc -threaded --make geturls.hs

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar)
import qualified Data.ByteString as B
import GetURL (getURL)
import Text.Printf (printf)
import TimeIt (timeit)

data Async a = Async (MVar a)

async ∷ IO a → IO (Async a)
async action = do
  var ← newEmptyMVar
  forkIO (do r ← action; putMVar var r)
  return (Async var)

wait ∷ Async a → IO a
wait (Async var) = readMVar var

sites ∷ [String]
sites =
  [ "http://www.google.com",
    "http://www.bing.com",
    "http://www.yahoo.com",
    "http://www.wikipedia.com/wiki/Spade",
    "http://www.wikipedia.com/wiki/Shovel"
  ]

timeDownload ∷ String → IO ()
timeDownload url = do
  (page, time) ← timeit $ getURL url
  printf "downloaded: %s (%d bytes, %.2fs)\n" url (B.length page) time

main ∷ IO ()
main = do
  as ← mapM (async . timeDownload) sites
  mapM_ wait as
