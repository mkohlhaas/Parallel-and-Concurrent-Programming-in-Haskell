-- import ConcurrentUtils (forkFinally)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, newTChan, newTVar, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.Exception ()
import Control.Monad (forever, join, void)
import Network (PortID (PortNumber), accept, listenOn, withSocketsDo)
import System.IO (BufferMode (LineBuffering), Handle, hClose, hGetLine, hPrint, hPutStrLn, hSetBuffering)
import Text.Printf (hPrintf, printf)

main ∷ IO ()
main = withSocketsDo $ do
  sock ← listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  factor ← newTVarIO 2
  void $
    forever $ do
      (handle, host, port) ← accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle factor) (\_ → hClose handle)

port ∷ Int
port = 44444

talk ∷ Handle → TVar Integer → IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c ← atomically newTChan
  race (server h factor c) (receive h c)
  return ()

receive ∷ Handle → TChan String → IO ()
receive h c = forever $ do
  line ← hGetLine h
  atomically $ writeTChan c line

server ∷ Handle → TVar Integer → TChan String → IO ()
server h factor c = do
  f ← readTVarIO factor
  hPrintf h "Current factor: %d\n" f
  loop f
  where
    loop f = do
      join
        ( atomically $
            do
              f' ← readTVar factor
              if f /= f'
                then pure (newfactor f')
                else do
                  l ← readTChan c
                  pure (command f l)
        )

    newfactor f = do
      hPrintf h "new factor: %d\n" f
      loop f

    command f s =
      case s of
        "end" → hPutStrLn h ("Thank you for using the " ++ "Haskell doubling service.")
        '*' : s → do
          atomically $ writeTVar factor (read s ∷ Integer)
          loop f
        line → do
          hPrint h (f * (read line ∷ Integer))
          loop f
