import Control.Exception
import Control.Monad.Par.Scheds.Trace
import Data.Maybe
import Sudoku
import System.Environment

main ∷ IO ()
main = do
  [f] ← getArgs
  grids ← fmap lines $ readFile f

  let (as, bs) = splitAt (length grids `div` 2) grids

  print $
    length $
      filter isJust $
        runPar $ do
          i1 ← new
          i2 ← new
          fork $ put i1 (map solve as)
          fork $ put i2 (map solve bs)
          as' ← get i1
          bs' ← get i2
          return (as' ++ bs')
