{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative ()
import Control.Concurrent (getNumCapabilities)
import Control.Exception (SomeException, try)
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.Par.Class as P hiding (runParIO)
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
  n ← getNumCapabilities
  runParIO (unE (find s d)) >>= print

find ∷ String → FilePath → EParIO (Maybe FilePath)
find s d = do
  fs ← liftIO $ getDirectoryContents d
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
      r ← get a
      case r of
        Nothing → loop as
        Just a → return (Just a)

subfind ∷ String → FilePath → ([EVar (Maybe FilePath)] → EParIO (Maybe FilePath)) → [EVar (Maybe FilePath)] → EParIO (Maybe FilePath)
subfind s p inner asyncs = do
  isdir ← liftIO $ doesDirectoryExist p
  if not isdir
    then inner asyncs
    else do
      r ← new
      liftPar $ P.fork (putResult (find s p) r)
      inner (r : asyncs)

-- An exception-handling version of the ParIO monad.  Exceptions from
-- IO computations are caught in liftIO, and propagated in the EParIO
-- monad.  An EVar is like an IVar, but can also contain an exception,
-- which is propagated by 'get'.  Instead of 'put' we have
-- 'putResult', which runs an EParIO and puts the result (or an
-- exception) into an EVar.
--

newtype EParIO a = E {unE ∷ ParIO (Either SomeException a)}

instance Functor EParIO where
  fmap f e = e >>= return . f

instance Applicative EParIO where
  pure = return
  (<*>) = ap

instance Monad EParIO where
  return a = E (return (Right a))
  E m >>= k = E $ do
    r ← m
    case r of
      Left e → return (Left e)
      Right a → unE (k a)

instance MonadIO EParIO where
  liftIO io = E $ liftIO (try io)

liftPar ∷ ParIO a → EParIO a
liftPar p = E $ p >>= return . Right

type EVar a = IVar (Either SomeException a)

new ∷ EParIO (EVar a)
new = liftPar P.new

get ∷ EVar a → EParIO a
get evar = E $ P.get evar

putResult ∷ EParIO a → EVar a → ParIO ()
putResult (E e) var = do e >>= P.put_ var
