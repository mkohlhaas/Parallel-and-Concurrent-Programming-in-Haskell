{-# LANGUAGE BangPatterns #-}

import Control.Exception (getMaskingState, throwIO, try)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import System.IO.Error (isDoesNotExistError)

main ∷ IO ()
main = do
  fs ← getArgs
  let loop !n [] = return n
      loop !n (f : fs) = do
        getMaskingState >>= print
        r ← Control.Exception.try (openFile f ReadMode)
        case r of
          Left e
            | isDoesNotExistError e → loop n fs
            | otherwise → throwIO e
          Right h → do
            s ← hGetContents h
            loop (n + length (lines s)) fs

  n ← loop 0 fs
  print n
