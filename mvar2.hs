import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)

main ∷ IO ()
main = do
  m ← newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r ← takeMVar m
  print r
  r ← takeMVar m
  print r
