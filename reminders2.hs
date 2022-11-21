import Control.Concurrent (forkIO, threadDelay)
import Control.Monad ()
import Text.Printf (printf)

main ∷ IO ()
main = loop
  where
    loop = do
      s ← getLine
      if s == "exit"
        then pure ()
        else do
          forkIO $ setReminder s
          loop

setReminder ∷ String → IO ()
setReminder s = do
  let t = read s ∷ Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10 ^ 6 * t)
  printf "%d seconds is up! BING!\BEL\n" t
