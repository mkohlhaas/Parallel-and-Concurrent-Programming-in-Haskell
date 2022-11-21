import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)

type Name = String

type PhoneNumber = String

type PhoneBook = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new ∷ IO PhoneBookState
new = do
  m ← newMVar Map.empty
  return (PhoneBookState m)

insert ∷ PhoneBookState → Name → PhoneNumber → IO ()
insert (PhoneBookState m) name number = do
  book ← takeMVar m
  putMVar m (Map.insert name number book)

lookup ∷ PhoneBookState → Name → IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book ← takeMVar m
  putMVar m book
  return (Map.lookup name book)

main ∷ IO ()
main = do
  s ← new
  sequence_ [insert s ("name" ++ show n) (show n) | n ← [1 .. 10000]]
  lookup s "name999" >>= print
  lookup s "unknown" >>= print
