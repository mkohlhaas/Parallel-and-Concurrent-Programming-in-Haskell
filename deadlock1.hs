import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)

main ∷ IO ()
main = do
  lock ← newEmptyMVar
  complete ← newEmptyMVar
  forkIO $ takeMVar lock `finally` putMVar complete ()
  takeMVar complete
