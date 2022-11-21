{-# LANGUAGE OverloadedStrings #-}

import Control.DeepSeq (NFData (..))
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

timetable ∷ [Person] → [Talk] → Int → Int → [TimeTable]
timetable people allTalks maxTrack maxSlot =
  generate 0 0 [] [] allTalks allTalks
  where
    generate ::
      Int → ------- current slot number
      Int → ------- current track number
      [[Talk]] → -- slots allocated so far
      [Talk] → ---- talks in this slot
      [Talk] → ---- talks that can be allocated in this slot
      [Talk] → ---- all talks remaining to be allocated
      [TimeTable] -- all possible solutions
    generate slotNo trackNo slots slot slotTalks talks
      | slotNo == maxSlot = [slots] -- <1>
      | trackNo == maxTrack =
        generate (slotNo + 1) 0 (slot : slots) [] talks talks
      | otherwise =
        concat
          [ generate slotNo (trackNo + 1) slots (t : slot) slotTalks' talks'
            | (t, ts) ← selects slotTalks,
              let clashesWithT = Map.findWithDefault [] t clashes,
              let slotTalks' = filter (`notElem` clashesWithT) ts,
              let talks' = filter (/= t) talks
          ]

    clashes ∷ Map Talk [Talk]
    clashes =
      Map.fromListWith
        union
        [ (t, ts)
          | s ← people,
            (t, ts) ← selects (talks s)
        ]

selects ∷ [a] → [(a, [a])]
selects = go []
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

test = timetable testPersons cs 2 2
  where
    cs@[c1, c2, c3, c4] = map Talk [1 .. 4]

    testPersons =
      [ Person "P" [c1, c2],
        Person "Q" [c2, c3],
        Person "R" [c3, c4],
        Person "S" [c1, c4]
      ]

test ∷ [TimeTable]

test2 n m = timetable testPersons cs m n
  where
    cs = map Talk [1 .. (n * m)]

    testPersons =
      [ Person "1" (take n cs)
      ]
