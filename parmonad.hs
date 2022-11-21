import Control.Exception ()
import Control.Monad.Par.Scheds.Trace (fork, get, new, put, runPar)
import System.Environment (getArgs)

-- NB. using Trace here, Direct is too strict and forces the fibs in
-- the parent; see https://github.com/simonmar/monad-par/issues/27

fib ∷ Integer → Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

main ∷ IO ()
main = do
  args ← getArgs
  let [n, m] = map read args
  print $
    runPar $ do
      i ← new
      j ← new
      fork (put i (fib n))
      fork (put j (fib m))
      a ← get i
      b ← get j
      return (a + b)
