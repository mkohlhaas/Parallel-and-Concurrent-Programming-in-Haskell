import ConcurrentUtils (forkFinally)
import Control.Concurrent (forkIO)
import Control.Exception ()
import Control.Monad (forever, void)
import Network (PortID (PortNumber), accept, listenOn, withSocketsDo)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hGetLine, hPrint, hPutStrLn, hSetBuffering)
import Text.Printf (printf)

main ∷ IO ()
main = withSocketsDo $ do
  sock ← listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  void $
    forever $ do
      (handle, host, port) ← accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle) (\_ → hClose handle)

port ∷ Int
port = 44444

talk ∷ Handle → IO ()
talk h = do
  hSetBuffering h LineBuffering
  loop
  where
    loop = do
      line ← hGetLine h
      if line == "end"
        then hPutStrLn h ("Thank you for using the " ++ "Haskell doubling service.")
        else do
          hPrint h (2 * (read line ∷ Integer))
          loop
