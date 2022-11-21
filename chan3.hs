import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (mask_)

type Stream a = MVar (Item a)

data Item a = Item a (Stream a)

data Chan a
  = Chan
      (MVar (Stream a))
      (MVar (Stream a))

newChan ∷ IO (Chan a)
newChan = do
  hole ← newEmptyMVar
  readVar ← newMVar hole
  writeVar ← newMVar hole
  return (Chan readVar writeVar)

wrongWriteChan ∷ Chan a → a → IO ()
wrongWriteChan (Chan _ writeVar) val = do
  newHole ← newEmptyMVar
  modifyMVar_ writeVar $ \oldHole → do
    putMVar oldHole (Item val newHole) -- <1>
    return newHole -- <2>

writeChan ∷ Chan a → a → IO ()
writeChan (Chan _ writeVar) val = do
  newHole ← newEmptyMVar
  mask_ $ do
    oldHole ← takeMVar writeVar
    putMVar oldHole (Item val newHole)
    putMVar writeVar newHole

readChan ∷ Chan a → IO a
readChan (Chan readVar _) = do
  modifyMVar readVar $ \stream → do
    Item val tail ← readMVar stream
    return (tail, val)

main ∷ IO ()
main = do
  c ← newChan
  writeChan c 'a'
  readChan c >>= print
