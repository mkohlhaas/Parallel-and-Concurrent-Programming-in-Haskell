import Control.Concurrent ()
import Control.Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Par.Class (ParFuture (get), ParIVar (fork, new, put))
import Control.Monad.Par.IO (IVar, ParIO, runParIO)
import Data.List (sort)
import GHC.Conc (getNumCapabilities)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf ()

main ∷ IO ()
main = do
  [s, d] ← getArgs
  runParIO (find s d) >>= print

find ∷ String → FilePath → ParIO (Maybe FilePath)
find s d = do
  fs ← liftIO $ getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if any (== s) fs'
    then return (Just (d </> s))
    else do
      let ps = map (d </>) fs'
      foldr (subfind s) dowait ps []
  where
    dowait vs = loop (reverse vs)

    loop [] = return Nothing
    loop (v : vs) = do
      r ← get v
      case r of
        Nothing → loop vs
        Just a → return (Just a)

subfind ∷ String → FilePath → ([IVar (Maybe FilePath)] → ParIO (Maybe FilePath)) → [IVar (Maybe FilePath)] → ParIO (Maybe FilePath)
subfind s p inner ivars = do
  isdir ← liftIO $ doesDirectoryExist p
  if not isdir
    then inner ivars
    else do
      v ← new
      fork (find s p >>= put v)
      inner (v : ivars)
