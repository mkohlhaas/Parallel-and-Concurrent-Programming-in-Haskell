import Control.Concurrent ()
import Control.Concurrent.Async (Async, wait, withAsync)
import Data.List (sort)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))

main ∷ IO ()
main = do
  [s, d] ← getArgs
  find s d >>= print

find ∷ String → FilePath → IO (Maybe FilePath)
find s d = do
  fs ← getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else do
      let ps = map (d </>) fs'
      foldr (subfind s) dowait ps []
  where
    dowait as = loop (reverse as)
    loop [] = return Nothing
    loop (a : as) = do
      r ← wait a
      case r of
        Nothing → loop as
        Just a → return (Just a)

subfind ∷ String → FilePath → ([Async (Maybe FilePath)] → IO (Maybe FilePath)) → [Async (Maybe FilePath)] → IO (Maybe FilePath)
subfind s p inner asyncs = do
  isdir ← doesDirectoryExist p
  if not isdir
    then inner asyncs
    else withAsync (find s p) $ \a → inner (a : asyncs)
