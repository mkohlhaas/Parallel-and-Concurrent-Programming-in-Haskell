import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, threadDelay)
import Control.Exception as E (SomeException, try)

main ∷ IO ()
main = do
  lock ← newEmptyMVar
  forkIO $ do r ← try (takeMVar lock); print (r ∷ Either SomeException ())
  threadDelay 1000000
  print (lock == lock)
