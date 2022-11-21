import Control.Parallel ()
import Control.Parallel.Strategies (Strategy, rpar, using)
import System.Environment ()
import Text.Printf ()

fib ∷ Integer → Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

evalPair ∷ Strategy a → Strategy b → Strategy (a, b)
evalPair sa sb (a, b) = do
  a' ← sa a
  b' ← sb b
  return (a', b')

parPair ∷ Strategy (a, b)
parPair = evalPair rpar rpar

main ∷ IO ()
main = print pair
  where
    pair = (fib 35, fib 36) `using` parPair
