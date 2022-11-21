{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-unused-imports #-}

-- -Wall

-- A module for stream processing built on top of Control.Monad.Par.
-- In the future may want to look into the stream interface used by the stream fusion framework.

module Stream
  ( Stream,
    streamFromList,
    streamMap,
    streamFold,
    streamFilter,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad.Par.Scheds.Trace as P (IVar, Par, fork, get, new, put, put_)

data IList a
  = Nil
  | Cons a (IVar (IList a))

type Stream a = IVar (IList a)

instance NFData a ⇒ NFData (IList a) where
  --  rnf Nil = r0
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b

streamFromList ∷ NFData a ⇒ [a] → Par (Stream a)
streamFromList xs = do
  var ← new
  fork $ loop xs var
  return var
  where
    loop [] var = put var Nil
    loop (x : xs) var = do
      tail ← new
      put var (Cons x tail)
      loop xs tail

streamMap ∷ NFData b ⇒ (a → b) → Stream a → Par (Stream b)
streamMap fn instrm = do
  outstrm ← new
  fork $ loop instrm outstrm
  return outstrm
  where
    loop instrm outstrm = do
      ilst ← get instrm
      case ilst of
        Nil → put outstrm Nil
        Cons h t → do
          newtl ← new
          put outstrm (Cons (fn h) newtl)
          loop t newtl

-- Reduce a stream to a single value.
-- This function will not return until it reaches the end-of-stream.
streamFold ∷ (a → b → a) → a → Stream b → Par a
streamFold fn !acc instrm = do
  ilst ← get instrm
  case ilst of
    Nil → return acc
    Cons h t → streamFold fn (fn acc h) t

streamFilter ∷ NFData a ⇒ (a → Bool) → Stream a → Par (Stream a)
streamFilter p instr = do
  outstr ← new
  fork $ loop instr outstr
  return outstr
  where
    loop instr outstr = do
      v ← get instr
      case v of
        Nil → put outstr Nil
        Cons x instr'
          | p x → do
            tail ← new
            put_ outstr (Cons x tail)
            loop instr' tail
          | otherwise → do
            loop instr' outstr
