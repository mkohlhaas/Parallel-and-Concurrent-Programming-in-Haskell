{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ < 706
import ConcurrentUtils (forkFinally)
#endif
import Control.Concurrent (ThreadId, forkFinally, throwTo)
import Control.Concurrent.STM (STM, TMVar, atomically, newEmptyTMVarIO, orElse, putTMVar, readTMVar, retry, throwSTM)
import Control.Exception (AsyncException (ThreadKilled), SomeException, throwTo)
import qualified Data.ByteString as B
import GetURL (getURL)
import Text.Printf (printf)

data Async a = Async ThreadId (TMVar (Either SomeException a))

async ∷ IO a → IO (Async a)
async action = do
  var ← newEmptyTMVarIO
  t ← forkFinally action (atomically . putTMVar var)
  return (Async t var)

waitCatch ∷ Async a → IO (Either SomeException a)
waitCatch = atomically . waitCatchSTM

waitCatchSTM ∷ Async a → STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var

waitSTM ∷ Async a → STM a
waitSTM a = do
  r ← waitCatchSTM a
  case r of
    Left e → throwSTM e
    Right a → return a

wait ∷ Async a → IO a
wait = atomically . waitSTM

cancel ∷ Async a → IO ()
cancel (Async t _) = throwTo t ThreadKilled

waitEither ∷ Async a → Async b → IO (Either a b)
waitEither a b =
  atomically $
    fmap Left (waitSTM a)
      `orElse` fmap Right (waitSTM b)

waitAny ∷ [Async a] → IO a
waitAny asyncs =
  atomically $ foldr orElse retry $ map waitSTM asyncs

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
  let download url = do
        r ← getURL url
        return (url, r)

  as ← mapM (async . download) sites

  (url, r) ← waitAny as
  printf "%s was first (%d bytes)\n" url (B.length r)
  mapM_ wait as
