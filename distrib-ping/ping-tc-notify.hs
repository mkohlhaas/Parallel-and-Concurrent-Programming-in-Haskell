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
  mapM_ monitor ps
  mypid ← getSelfPid
  ports ← forM ps $ \pid → do
    say $ printf "pinging %s" (show pid)
    (sendport, recvport) ← newChan
    send pid (Ping sendport)
    return recvport
  let loop [] = return ()
      loop (port : ps) = do
        receiveWait
          [ match $ \(ProcessMonitorNotification ref pid reason) → do
              say (show pid ++ " died: " ++ show reason)
              loop (port : ps),
            matchChan port $ \p → do
              say "pong on channel"
              loop ps
          ]
  loop ports
  say "All pongs successfully received"
  terminate

main ∷ IO ()
main = distribMain master Main.__remoteTable
