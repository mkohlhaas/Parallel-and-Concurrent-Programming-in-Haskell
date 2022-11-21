import Control.Exception
import Control.Monad.Par
import Data.Maybe
import Sudoku
import System.Environment

main ∷ IO ()
main = do
  [f] ← getArgs
  grids ← fmap lines $ readFile f
  print (length (filter isJust (runPar $ parMap solve grids)))
