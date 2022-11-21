-- (c) Simon Marlow 2011, see the file LICENSE for copying terms.
--
-- Sample geturls.hs (CEFP summer school notes, 2011)
--
-- Downloading multiple URLs concurrently, timing the downloads,
-- and the user may press 'q' to stop the downloads at any time.
--
-- Compile with:
--    ghc -threaded --make geturlscancel.hs

import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, readMVar, throwTo)
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwTo, try)
import Control.Monad (Monad (return), forever, mapM, mapM_, when)
import qualified Data.ByteString as B
import Data.Either (Either, rights)
import GetURL (getURL)
import System.IO (BufferMode (NoBuffering), IO, getChar, hSetBuffering, stdin)
import Text.Printf (printf)
import TimeIt (timeit)
import Prelude hiding (catch)

data Async a = Async ThreadId (MVar (Either SomeException a))

async ∷ IO a → IO (Async a)
async action = do
  m ← newEmptyMVar
  t ← forkIO (do r ← try action; putMVar m r)
  return (Async t m)

waitCatch ∷ Async a → IO (Either SomeException a)
waitCatch (Async _ var) = readMVar var

cancel ∷ Async a → IO ()
cancel (Async t var) = throwTo t ThreadKilled

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
  forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ do
      c ← getChar
      when (c == 'q') $ mapM_ cancel as
  rs ← mapM waitCatch as
  printf "%d/%d succeeded\n" (length (rights rs)) (length rs)
