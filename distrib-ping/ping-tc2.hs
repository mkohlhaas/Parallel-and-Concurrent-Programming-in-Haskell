{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Remote
import Text.Printf

data Message
  = Ping (SendPort Message)
  | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message

pingServer ∷ ProcessM ()
pingServer = do
  Ping chan ← expect
  mypid ← getSelfPid
  sendChannel chan (Pong mypid)

remotable ['pingServer]

master ∷ ProcessM ()
master = do
  peers ← getPeers
  let workers = findPeerByRole peers "WORKER"
  ps ← forM workers $ \nid → do
    say $ printf "spawning on %s" (show nid)
    spawn nid pingServer__closure
  mypid ← getSelfPid
  ports ← forM ps $ \pid → do
    say $ printf "pinging %s" (show pid)
    (sendport, recvport) ← newChannel
    send pid (Ping sendport)
    return recvport
  oneport ← mergePortsBiased ports
  waitForPongs oneport ps
  say "All pongs successfully received"
  terminate

waitForPongs ∷ ReceivePort Message → [ProcessId] → ProcessM ()
waitForPongs port [] = return ()
waitForPongs port ps = do
  m ← receiveChannel port
  case m of
    Pong p → waitForPongs port (filter (/= p) ps)
    _ → say "MASTER received ping" >> terminate


main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess

initialProcess ∷ String → ProcessM ()
initialProcess "WORKER" = receiveWait []
initialProcess "MASTER" = master
