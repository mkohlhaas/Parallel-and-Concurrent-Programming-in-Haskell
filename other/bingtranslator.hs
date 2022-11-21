import BingTranslate as Bing (detectLanguage, getLanguages, translateText)
import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, readMVar, throwTo)
import Control.Exception (AsyncException (ThreadKilled), SomeException, catch, throwTo)
import Control.Monad (Functor (fmap), Monad (return), forM_)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import System.Environment (getArgs)
import Text.Printf (printf)
import Prelude hiding (catch)

main ∷ IO ()
main = do
  [text] ← fmap (fmap (B.unpack . UTF8.fromString)) getArgs
  languages ← Bing.getLanguages
  fromLang ← Bing.detectLanguage text
  printf "\"%s\" appears to be in language \"%s\"\n" text fromLang
  forM_ (filter (/= fromLang) languages) $ \toLang → do
    str ← Bing.translateText text fromLang toLang
    printf "%s: %s\n" toLang str

data Async a = Async ThreadId (MVar (Either SomeException a))

async ∷ IO a → IO (Async a)
async action = do
  var ← newEmptyMVar
  t <-
    forkIO
      ( (do r ← action; putMVar var (Right r))
          `catch` \e → putMVar var (Left e)
      )
  pure (Async t var)

wait ∷ Async a → IO (Either SomeException a)
wait (Async t var) = readMVar var

cancel ∷ Async a → IO ()
cancel (Async t var) = throwTo t ThreadKilled
