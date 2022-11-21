{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module WorkerSample where

import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import GHC.Generics (Generic)
import Text.Printf

class Send c a where
  (!) ∷ Serializable a ⇒ c → a → Process ()

instance Send ProcessId a where
  (!) = send

instance Send (SendPort a) a where
  (!) = sendChan

type Key = String -- should really use ByteString

type Value = String

data Request
  = Set Key Value
  | Get Key (SendPort (Maybe Value))
  deriving (Typeable, Generic)

instance Binary Request

worker ∷ Process ()
worker = go Map.empty
  where
    go store = do
      r ← expect
      case r of
        Set k v ->
          go (Map.insert k v store)
        Get k port → do
          port ! (Map.lookup k store)
          go store

remotable ['worker]
