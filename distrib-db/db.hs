{-# LANGUAGE OverloadedStrings #-}

import Control.Distributed.Process
import Control.Monad
import Control.Monad.IO.Class
import Database (Database, createDB, get, rcdata, set)
import DistribUtils
import System.IO

main = distribMain master rcdata

master ∷ [NodeId] → Process ()
master peers = do
  db ← createDB peers

  f ← liftIO $ readFile "Database.hs"
  let ws = words f

  zipWithM_ (set db) ws (tail ws)

  get db "module" >>= liftIO . print
  get db "xxxx" >>= liftIO . print

  forever $ do
    l ← liftIO $ do putStr "key: "; hFlush stdout; getLine
    when (not (null l)) $ do
      r ← get db l
      liftIO $ putStrLn ("response: " ++ show r)

  return ()
