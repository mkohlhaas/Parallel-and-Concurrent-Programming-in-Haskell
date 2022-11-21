import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Text.Printf (printf)

main ∷ IO ()
main =
  void $
    forever $ do
      s ← getLine
      forkIO $ setReminder s

setReminder ∷ String → IO ()
setReminder s = do
  let t = read s ∷ Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10 ^ 6 * t)
  printf "%d seconds is up! BING!\BEL\n" t
