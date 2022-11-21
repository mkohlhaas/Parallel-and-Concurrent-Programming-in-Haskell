import Control.Concurrent (newEmptyMVar, takeMVar)
import Control.Monad (void)

main ∷ IO ()
main = do
  m ← newEmptyMVar
  void $ takeMVar m
