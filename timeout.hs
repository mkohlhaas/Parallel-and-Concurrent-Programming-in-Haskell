{-# LANGUAGE DeriveDataTypeable #-}

import Control.Concurrent (forkIO, myThreadId, threadDelay, throwTo)
import Control.Exception (AsyncException (ThreadKilled), Exception, bracket, handleJust, throwTo)
import Data.Typeable (Typeable)
import Data.Unique (Unique, newUnique)

data Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
  show (Timeout _) = "timeout"

instance Exception Timeout

timeout ∷ Int → IO a → IO (Maybe a)
timeout t m
  | t < 0 = fmap Just m
  | t == 0 = return Nothing
  | otherwise = do
    pid ← myThreadId
    u ← newUnique
    let ex = Timeout u
    handleJust
      (\e → if e == ex then Just () else Nothing)
      (\_ → return Nothing)
      ( bracket
          ( forkIO $ do
              threadDelay t
              throwTo pid ex
          )
          (`throwTo` ThreadKilled)
          (\_ → fmap Just m)
      )

main ∷ IO ()
main =
  timeout
    200000
    (timeout 100000 $ timeout 300000 $ threadDelay 1000000)
    >>= print
