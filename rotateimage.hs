{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import System.Environment

main ∷ IO ()
main = do
  [n, f1, f2] ← getArgs
  runIL $ do
    (RGB v) ← readImage f1
    rotated ← computeP $ rotate (read n) v ∷ IL (Array F DIM3 Word8)
    writeImage f2 (RGB rotated)

rotate ∷ Double → Array F DIM3 Word8 → Array D DIM3 Word8
rotate deg g = fromFunction (Z :. y :. x :. k) f
  where
    sh@(Z :. y :. x :. k) = extent g

    !theta = pi / 180 * deg

    !st = sin theta
    !ct = cos theta

    !cy = fromIntegral y / 2 ∷ Double
    !cx = fromIntegral x / 2 ∷ Double

    f (Z :. i :. j :. k)
      | inShape sh old = g ! old
      | otherwise = 0
      where
        fi = fromIntegral i - cy
        fj = fromIntegral j - cx

        i' = round (st * fj + ct * fi + cy)
        j' = round (ct * fj - st * fi + cx)

        old = Z :. i' :. j' :. k
