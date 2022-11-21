module WindowManager where

import Control.Concurrent.STM
import Data.Map as Map
import Data.Maybe
import Data.Set as Set

data Window

instance Eq Window

instance Ord Window

data Desktop

instance Eq Desktop

instance Ord Desktop

type Display = Map Desktop (TVar (Set Window))

moveWindowSTM ∷ Display → Window → Desktop → Desktop → STM ()
moveWindowSTM disp win a b = do
  wa ← readTVar ma
  wb ← readTVar mb
  writeTVar ma (Set.delete win wa)
  writeTVar mb (Set.insert win wb)
  where
    ma = disp ! a
    mb = disp ! b

moveWindow ∷ Display → Window → Desktop → Desktop → IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b

swapWindows ∷ Display → Window → Desktop → Window → Desktop → IO ()
swapWindows disp w a v b = atomically $ do
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a

render ∷ Set Window → IO ()
render = undefined

type UserFocus = TVar Desktop

getWindows ∷ Display → UserFocus → STM (Set Window)
getWindows disp focus = do
  desktop ← readTVar focus
  readTVar (disp ! desktop)

renderThread ∷ Display → UserFocus → IO ()
renderThread disp focus = do
  wins ← atomically $ getWindows disp focus
  loop wins
  where
    loop wins = do
      render wins
      next ← atomically $ do
        wins' ← getWindows disp focus
        if (wins == wins')
          then retry
          else return wins'
      loop next
