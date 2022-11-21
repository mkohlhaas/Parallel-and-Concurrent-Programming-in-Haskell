{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
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

master ∷ Process ()
master = do
  node ← getSelfNode

  say $ printf "spawning on %s" (show node)
  pid ← spawn node $(mkStaticClosure 'pingServer)

  mypid ← getSelfPid
  say $ printf "sending ping to %s" (show pid)

  withMonitor pid $ \_ → do
    send pid (Pong mypid)
    receiveWait
      [ match $ \(Pong _) → do
          say "pong."
          terminate,
        match $ \(ProcessMonitorNotification _ref deadpid reason) → do
          say (printf "process %s died: %s" (show deadpid) (show reason))
          terminate
      ]

main ∷ IO ()
main = distribMain (\_ → master) Main.__remoteTable
