import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad ()
import qualified Data.ByteString as B
import GetURL (getURL)
import Text.Printf (printf)
import TimeIt ()

data Async a = Async (MVar (Either SomeException a))

async ∷ IO a → IO (Async a)
async action = do
  var ← newEmptyMVar
  forkIO (do r ← try action; putMVar var r)
  return (Async var)

waitCatch ∷ Async a → IO (Either SomeException a)
waitCatch (Async var) = readMVar var

wait ∷ Async a → IO a
wait a = do
  r ← waitCatch a
  case r of
    Left e → throwIO e
    Right a → return a

waitEither ∷ Async a → Async b → IO (Either a b)
waitEither a b = do
  m ← newEmptyMVar
  forkIO $ do r ← try (fmap Left (wait a)); putMVar m r
  forkIO $ do r ← try (fmap Right (wait b)); putMVar m r
  wait (Async m)

waitAny ∷ [Async a] → IO a
waitAny as = do
  m ← newEmptyMVar
  let forkwait a = forkIO $ do r ← try (wait a); putMVar m r
  mapM_ forkwait as
  wait (Async m)

sites ∷ [[Char]]
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
