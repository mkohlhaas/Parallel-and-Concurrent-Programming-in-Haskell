{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.Async (Async, wait, withAsync)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (finally)
import Data.IORef ()
import Data.List (sort)
import Data.Maybe (isJust)
import GHC.Conc (getNumCapabilities)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

main ∷ IO ()
main = do
  [n, s, d] ← getArgs
  sem ← newNBSem (read n)
  find sem s d >>= print

find ∷ NBSem → String → FilePath → IO (Maybe FilePath)
find sem s d = do
  fs ← getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else do
      let ps = map (d </>) fs' -- <1>
      foldr (subfind sem s) dowait ps [] -- <2>
  where
    dowait as = loop (reverse as) -- <3>
    loop [] = return Nothing
    loop (a : as) = do
      -- <4>
      r ← wait a -- <5>
      case r of
        Nothing → loop as -- <6>
        Just a → return (Just a) -- <7>
        -- >>

subfind ∷ NBSem → String → FilePath → ([Async (Maybe FilePath)] → IO (Maybe FilePath)) → [Async (Maybe FilePath)] → IO (Maybe FilePath)
subfind sem s p inner asyncs = do
  isdir ← doesDirectoryExist p
  if not isdir
    then inner asyncs
    else do
      q ← tryAcquireNBSem sem -- <1>
      if q
        then do
          let dofind = find sem s p `finally` releaseNBSem sem -- <2>
          withAsync dofind $ \a → inner (a : asyncs)
        else do
          r ← find sem s p -- <3>
          case r of
            Nothing → inner asyncs
            Just _ → return r

newtype NBSem = NBSem (MVar Int)

newNBSem ∷ Int → IO NBSem
newNBSem i = do
  m ← newMVar i
  return (NBSem m)

tryAcquireNBSem ∷ NBSem → IO Bool
tryAcquireNBSem (NBSem m) =
  modifyMVar m $ \i ->
    if i == 0
      then return (i, False)
      else let !z = i -1 in return (z, True)

releaseNBSem ∷ NBSem → IO ()
releaseNBSem (NBSem m) =
  modifyMVar m $ \i ->
    let !z = i + 1 in return (z, ())
