{-# OPTIONS_GHC -Wall #-}

import Data.List (sort)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

main ∷ IO ()
main = do
  [s, d] ← getArgs
  r ← find s d
  print r

find ∷ String → FilePath → IO (Maybe FilePath)
find s d = do
  fs ← getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else loop fs'
  where
    loop [] = return Nothing
    loop (f : fs) = do
      let d' = d </> f
      isdir ← doesDirectoryExist d'
      if isdir
        then do
          r ← find s d'
          case r of
            Just _ → return r
            Nothing → loop fs
        else loop fs
