{-# LANGUAGE BangPatterns #-}

import Control.Exception as E (getMaskingState, handle, throwIO)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import System.IO.Error (isDoesNotExistError)

main ∷ IO ()
main = do
  fs ← getArgs
  let loop !n [] = return n
      loop !n (f : fs) =
        handle
          ( \e ->
              if isDoesNotExistError e
                then loop n fs
                else throwIO e
          )
          $ do
            getMaskingState >>= print
            h ← openFile f ReadMode
            s ← hGetContents h
            loop (n + length (lines s)) fs

  n ← loop 0 fs
  print n
