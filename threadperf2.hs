import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM)

numThreads ∷ Int
numThreads = 1000000

main ∷ IO ()
main = do
  ms ← replicateM numThreads $ do
    m ← newEmptyMVar
    forkIO (putMVar m ())
    return m
  mapM_ takeMVar ms
