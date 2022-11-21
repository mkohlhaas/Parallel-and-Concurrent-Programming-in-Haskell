import Control.Concurrent
import Control.Monad
import System.IO
import System.Random
import System.Timeout
import Text.Printf

data Msg = C Char | Time

main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  m ← newEmptyMVar
  scorem ← newMVar 0
  forkIO $
    forever $ do
      score ← readMVar scorem
      threadDelay (truncate (fromIntegral 1000000 * (0.9 ^ score)))
      putMVar m Time
  forkIO $ forever (do c ← getChar; putMVar m (C c))
  loop m scorem ""
  where
    update m old new = do
      putStr (replicate n '\8')
      putStr (replicate n ' ')
      putStr (replicate n '\8')
      putStr new
      where
        n = length old
    loop ∷ MVar Msg → MVar Int → String → IO ()
    loop m scorem s
      | length s == 10 = do
        score ← readMVar scorem
        printf "\nGAME OVER.  SCORE = %d\n" score
      | otherwise = do
        x ← takeMVar m
        case x of
          C c → do
            let new = filter (/= c) s
            update m s new
            modifyMVar_ scorem (return . (+ (length s - length new)))
            loop m scorem new
          Time → do
            c ← randomRIO ('0', '9')
            let new = s ++ [c]
            update m s new
            loop m scorem new
