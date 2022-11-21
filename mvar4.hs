import Control.Concurrent (forkIO, myThreadId, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Debug.Trace (traceEventIO)
import GHC.Conc (forkIO, labelThread, myThreadId)

main ∷ IO ()
main = do
  t ← myThreadId
  labelThread t "main"
  m ← newEmptyMVar
  t ← forkIO $ putMVar m 'a'
  labelThread t "a"
  t ← forkIO $ putMVar m 'b'
  labelThread t "b"
  traceEventIO "before takeMVar"
  takeMVar m
  void $ takeMVar m
