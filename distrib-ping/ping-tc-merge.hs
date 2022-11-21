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

data Message = Ping (SendPort ProcessId)
  deriving (Typeable, Generic)

instance Binary Message

pingServer ∷ Process ()
pingServer = do
  Ping chan ← expect
  say $ printf "ping received from %s" (show chan)
  mypid ← getSelfPid
  sendChan chan mypid

remotable ['pingServer]

master ∷ [NodeId] → Process ()
master peers = do
  ps ← forM peers $ \nid → do
    say $ printf "spawning on %s" (show nid)
    spawn nid $(mkStaticClosure 'pingServer)

  ports ← forM ps $ \pid → do
    say $ printf "pinging %s" (show pid)
    (sendport, recvport) ← newChan
    send pid (Ping sendport)
    return recvport

  oneport ← mergePortsBiased ports
  waitForPongs oneport ps
  say "All pongs successfully received"
  terminate

waitForPongs ∷ ReceivePort ProcessId → [ProcessId] → Process ()
waitForPongs _ [] = return ()
waitForPongs port ps = do
  pid ← receiveChan port
  waitForPongs port (filter (/= pid) ps)

main ∷ IO ()
main = distribMain master Main.__remoteTable
