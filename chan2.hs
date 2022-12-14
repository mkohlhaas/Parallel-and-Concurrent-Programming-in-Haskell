import Control.Concurrent (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)

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

writeChan ∷ Chan a → a → IO ()
writeChan (Chan _ writeVar) val = do
  newHole ← newEmptyMVar
  oldHole ← takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole

readChan ∷ Chan a → IO a
readChan (Chan readVar _) = do
  stream ← takeMVar readVar
  Item val tail ← readMVar stream
  putMVar readVar tail
  return val

dupChan ∷ Chan a → IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole ← takeMVar writeVar
  putMVar writeVar hole
  newReadVar ← newMVar hole
  return (Chan newReadVar writeVar)

unGetChan ∷ Chan a → a → IO ()
unGetChan (Chan readVar _) val = do
  newReadEnd ← newEmptyMVar
  readEnd ← takeMVar readVar
  putMVar newReadEnd (Item val readEnd)
  putMVar readVar newReadEnd

main ∷ IO ()
main = do
  c ← newChan
  writeChan c 'a'
  readChan c >>= print
  c2 ← dupChan c
  writeChan c 'b'
  readChan c >>= print
  readChan c2 >>= print
