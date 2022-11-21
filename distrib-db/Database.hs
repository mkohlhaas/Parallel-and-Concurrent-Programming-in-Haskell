module Database
  ( Database,
    Key,
    Value,
    createDB,
    get,
    set,
    rcdata,
  )
where

import Control.Distributed.Process
import Data.Map (Map)
import qualified Data.Map as Map

type Key = String

type Value = String

type Database = ProcessId

createDB ∷ [NodeId] → Process Database
createDB nodes = error "not implemented!"

set ∷ Database → Key → Value → Process ()
set db k v = error "not implemented!"

get ∷ Database → Key → Process (Maybe Value)
get db k = error "not implemented!"

rcdata ∷ RemoteTable → RemoteTable
rcdata = id

-- For the exercise, change this to include your
-- remote metadata, e.g. rcdata = Database.__remoteTable
