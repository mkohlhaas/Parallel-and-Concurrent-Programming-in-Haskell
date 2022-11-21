import Control.Concurrent (forkIO)
import Control.Monad (replicateM_)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main ∷ IO ()
main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 100000 (putChar 'A'))
  replicateM_ 100000 (putChar 'B')
