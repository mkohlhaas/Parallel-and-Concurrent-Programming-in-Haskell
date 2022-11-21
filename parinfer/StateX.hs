{-# LANGUAGE TupleSections #-}

--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module StateX (StateX, returnSX, eachSX, thenSX, toSX, putSX, getSX, useSX) where
import Data.Bifunctor (Bifunctor(first))

newtype StateX s a = MkSX (s -> a)

rep :: StateX s a -> s -> a
rep (MkSX f) = f

returnSX :: ((a1, b) -> a2) -> a1 -> StateX b a2
returnSX returnX x = MkSX (\s -> returnX (x, s))

eachSX :: (t1 -> ((t2, b) -> (a1, b)) -> a2) -> StateX s t1 -> (t2 -> a1) -> StateX s a2
eachSX eachX xSX f = MkSX (\s -> rep xSX s `eachX` first f)

thenSX :: (t1 -> ((t2, s1) -> a1) -> a2) -> StateX s2 t1 -> (t2 -> StateX s1 a1) -> StateX s2 a2
thenSX thenX xSX kSX = MkSX (\s -> rep xSX s `thenX` (\(x, s') -> rep (kSX x) s'))

toSX :: (t -> (a1 -> (a1, b)) -> a2) -> t -> StateX b a2
toSX eachX xX = MkSX (\s -> xX `eachX` (,s))

putSX :: (((), b) -> a) -> b -> StateX s a
putSX returnX s' = MkSX (\s -> returnX ((), s'))

getSX :: ((b, b) -> a) -> StateX b a
getSX returnX = MkSX (\s -> returnX (s, s))

useSX :: (t1 -> ((a, b) -> a) -> t2) -> s -> StateX s t1 -> t2
useSX eachX s xSX = rep xSX s `eachX` fst
