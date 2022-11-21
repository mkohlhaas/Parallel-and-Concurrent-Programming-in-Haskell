{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq (NFData (..))
import Control.Monad.Par (NFData, parMapM, runPar)
import Data.List (nub, union)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace ()
import System.Environment (getArgs)
import System.Random (Random (randomRs), RandomGen (split), StdGen, mkStdGen)

newtype Talk = Talk Int
  deriving (Eq, Ord)

instance NFData Talk where
  rnf (Talk x) = x `seq` ()

instance Show Talk where
  show (Talk t) = show t

data Person = Person
  { name ∷ String,
    talks ∷ [Talk]
  }
  deriving (Show)

type TimeTable = [[Talk]]

search ∷ (partial → Maybe solution) → (partial → [partial]) → partial → [solution]
search finished refine emptysoln = generate emptysoln
  where
    generate partial
      | Just soln ← finished partial = [soln]
      | otherwise = concat (map generate (refine partial))

parsearch ::
  NFData solution =>
  Int ->
  (partial → Maybe solution) → -- finished?
  (partial → [partial]) → -- refine a solution
  partial → -- initial solution
  [solution]
parsearch maxdepth finished refine emptysoln =
  runPar $ generate 0 emptysoln
  where
    generate d partial
      | d >= maxdepth =
        return (search finished refine partial)
    generate d partial
      | Just soln ← finished partial = return [soln]
      | otherwise = do
        solnss ← parMapM (generate (d + 1)) (refine partial)
        return (concat solnss)

type Partial = (Int, Int, [[Talk]], [Talk], [Talk], [Talk])

timetable ∷ [Person] → [Talk] → Int → Int → [TimeTable]
timetable people allTalks maxTrack maxSlot =
  parsearch 3 finished refine emptysoln
  where
    emptysoln = (0, 0, [], [], allTalks, allTalks)

    finished (slotNo, trackNo, slots, slot, slotTalks, talks)
      | slotNo == maxSlot = Just slots
      | otherwise = Nothing

    clashes ∷ Map Talk [Talk]
    clashes =
      Map.fromListWith
        union
        [ (t, ts)
          | s ← people,
            (t, ts) ← selects (talks s)
        ]

    refine (slotNo, trackNo, slots, slot, slotTalks, talks)
      | trackNo == maxTrack = [(slotNo + 1, 0, slot : slots, [], talks, talks)]
      | otherwise =
        [ (slotNo, trackNo + 1, slots, t : slot, slotTalks', talks')
          | (t, ts) ← selects slotTalks,
            let clashesWithT = Map.findWithDefault [] t clashes,
            let slotTalks' = filter (`notElem` clashesWithT) ts,
            let talks' = filter (/= t) talks
        ]

selects ∷ [a] → [(a, [a])]
selects xs0 = go [] xs0
  where
    go xs [] = []
    go xs (y : ys) = (y, xs ++ ys) : go (y : xs) ys

bench ∷ Int → Int → Int → Int → Int → StdGen → ([Person], [Talk], [TimeTable])
bench nslots ntracks ntalks npersons c_per_s gen =
  (persons, talks, timetable persons talks ntracks nslots)
  where
    total_talks = nslots * ntracks

    talks = map Talk [1 .. total_talks]
    persons = mkpersons npersons gen

    mkpersons ∷ Int → StdGen → [Person]
    mkpersons 0 g = []
    mkpersons n g = Person ('P' : show n) (take c_per_s cs) : rest
      where
        (g1, g2) = split g
        rest = mkpersons (n -1) g2
        cs = nub [talks !! n | n ← randomRs (0, ntalks -1) g]

main ∷ IO ()
main = do
  [a, b, c, d, e] ← fmap (fmap read) getArgs
  let g = mkStdGen 1001
  let (ss, cs, ts) = bench a b c d e g
  print ss
  print (length ts)

--   [ a, b ] ← fmap (fmap read) getArgs
--   print (head (test2 a b))

test ∷ [TimeTable]
test = timetable testPersons cs 2 2
  where
    cs@[c1, c2, c3, c4] = map Talk [1 .. 4]

    testPersons =
      [ Person "P" [c1, c2],
        Person "Q" [c2, c3],
        Person "R" [c3, c4]
      ]

test2 ∷ Int → Int → [TimeTable]
test2 n m = timetable testPersons cs m n
  where
    cs = map Talk [1 .. (n * m)]

    testPersons =
      [ Person "1" (take n cs)
      ]
