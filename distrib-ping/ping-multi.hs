{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Data.Binary
import Data.Typeable
import DistribUtils
import GHC.Generics (Generic)
import Text.Printf

data Message
  = Ping ProcessId
  | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message

pingServer ∷ Process ()
pingServer = do
  Ping from ← expect
  say $ printf "ping received from %s" (show from)
  mypid ← getSelfPid
  send from (Pong mypid)

remotable ['pingServer]

master ∷ [NodeId] → Process ()
master peers = do
  ps ← forM peers $ \nid → do
    say $ printf "spawning on %s" (show nid)
    spawn nid $(mkStaticClosure 'pingServer)

  mypid ← getSelfPid

  forM_ ps $ \pid → do
    say $ printf "pinging %s" (show pid)
    send pid (Ping mypid)

  waitForPongs ps
  say "All pongs successfully received"
  terminate

waitForPongs ∷ [ProcessId] → Process ()
waitForPongs [] = return ()
waitForPongs ps = do
  m ← expect
  case m of
    Pong p → waitForPongs (filter (/= p) ps)
    _ → say "MASTER received ping" >> terminate

main ∷ IO ()
main = distribMain master Main.__remoteTable
