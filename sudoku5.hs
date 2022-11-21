import Control.Exception ()
import Control.Parallel.Strategies (parList, rseq, using)
import Data.Maybe (isJust)
import Sudoku (solve)
import System.Environment (getArgs)

main ∷ IO ()
main = do
  [f] ← getArgs
  file ← readFile f
  let puzzles = lines file
  let solutions = map solve puzzles `using` parList rseq
  print (length (filter isJust solutions))
