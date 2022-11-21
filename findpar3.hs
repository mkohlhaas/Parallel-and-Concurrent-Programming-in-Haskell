{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

import CasIORef ()
import Control.Concurrent.Async (Async, wait, withAsync)
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ()
import Control.Exception (finally)
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.List (sort)
import Data.Maybe (isJust)
import GHC.Conc (getNumCapabilities)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

main ∷ IO ()
main = do
  [s, d] ← getArgs
  n ← getNumCapabilities
  sem ← newNBSem (if n == 1 then 0 else n * 4)
  find sem s d >>= print

find ∷ NBSem → String → FilePath → IO (Maybe FilePath)
find sem s d = do
  fs ← getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else do
      let ps = map (d </>) fs'
      foldr (subfind sem s) dowait ps []
  where
    dowait as = loop (reverse as)
    loop [] = return Nothing
    loop (a : as) = do
      r ← wait a
      case r of
        Nothing → loop as
        Just a → return (Just a)

subfind ∷ NBSem → String → FilePath → ([Async (Maybe FilePath)] → IO (Maybe FilePath)) → [Async (Maybe FilePath)] → IO (Maybe FilePath)
subfind sem s p inner asyncs = do
  isdir ← doesDirectoryExist p
  if isdir
    then do
      q ← tryWaitNBSem sem
      if q
        then withAsync (find sem s p `finally` signalNBSem sem) $ \a ->
          inner (a : asyncs)
        else do
          r ← find sem s p
          if isJust r then return r else inner asyncs
    else inner asyncs

newtype NBSem = NBSem (IORef Int)

newNBSem ∷ Int → IO NBSem
newNBSem i = do
  m ← newIORef i
  return (NBSem m)

tryWaitNBSem ∷ NBSem → IO Bool
tryWaitNBSem (NBSem m) = do
  atomicModifyIORef m $ \i ->
    if i == 0
      then (i, False)
      else let !z = i -1 in (z, True)

signalNBSem ∷ NBSem → IO ()
signalNBSem (NBSem m) =
  atomicModifyIORef m $ \i ->
    let !z = i + 1 in (z, ())
