{-# LANGUAGE TypeOperators #-}

--
-- A Mandelbrot set generator. Submitted by Simon Marlow as part of Issue #49.
--

import Config
import Control.Exception
import Control.Monad
import Data.Array.Accelerate as A hiding (size)
import Data.Array.Accelerate.Array.Data (ptrsOfArrayData)
import Data.Array.Accelerate.Array.Sugar (Array (..))
import Data.Array.Accelerate.IO
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.DevIL
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import Foreign.ForeignPtr
import Foreign.Ptr
import System.Environment
import System.IO.Unsafe
import Prelude as P

type Complex = (F, F)

type ComplexPlane = Array DIM2 Complex

mandelbrot ∷ F → F → F → F → Int → Int → Int → Acc (Array DIM2 (Complex, Int))
mandelbrot x y x' y' screenX screenY max_depth =
  iterate go zs0 !! max_depth
  where
    cs = genPlane x y x' y' screenX screenY
    zs0 = mkinit cs
    go ::
      Acc (Array DIM2 (Complex, Int)) ->
      Acc (Array DIM2 (Complex, Int))
    go = A.zipWith iter cs -- <3>

genPlane ∷ F → F → F → F → Int → Int → Acc ComplexPlane
genPlane lowx lowy highx highy viewx viewy =
  generate
    (constant (Z :. viewy :. viewx))
    ( \ix ->
        let pr = unindex2 ix
            x = A.fromIntegral (A.fst pr)
            y = A.fromIntegral (A.snd pr)
         in lift
              ( elowx + (x * exsize) / eviewx,
                elowy + (y * eysize) / eviewy
              )
    )
  where
    elowx, elowy, exsize, eysize, eviewx, eviewy ∷ Exp F
    elowx = constant lowx
    elowy = constant lowy
    exsize = constant (highx - lowx)
    eysize = constant (highy - lowy)
    eviewx = constant (P.fromIntegral viewx)
    eviewy = constant (P.fromIntegral viewy)

next ∷ Exp Complex → Exp Complex → Exp Complex
next c z = c `plus` (z `times` z)

plus ∷ Exp Complex → Exp Complex → Exp Complex
plus = lift2 f
  where
    f ∷ (Exp F, Exp F) → (Exp F, Exp F) → (Exp F, Exp F)
    f (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

times ∷ Exp Complex → Exp Complex → Exp Complex
times = lift2 f
  where
    f ∷ (Exp F, Exp F) → (Exp F, Exp F) → (Exp F, Exp F)
    f (ax, ay) (bx, by) = (ax * bx - ay * by, ax * by + ay * bx)

dot ∷ Exp Complex → Exp F
dot = lift1 f
  where
    f ∷ (Exp F, Exp F) → Exp F
    f (x, y) = x * x + y * y

iter ∷ Exp Complex → Exp (Complex, Int) → Exp (Complex, Int)
iter c p =
  let (z, i) = unlift p ∷ (Exp Complex, Exp Int)
      z' = next c z
   in (dot z' >* 4.0) ? (p, lift (z', i + 1))

mkinit ∷ Acc ComplexPlane → Acc (Array DIM2 (Complex, Int))
mkinit cs = A.map (lift1 f) cs
  where
    f ∷ (Exp F, Exp F) → ((Exp F, Exp F), Exp Int)
    f (x, y) = ((x, y), 0)

type RGBA = Word32

prettyRGBA ∷ Exp Int → Exp (Complex, Int) → Exp RGBA
prettyRGBA lIMIT s' = r + g + b + a
  where
    (_, s) = unlift s' ∷ (Exp (F, F), Exp Int)
    t = A.fromIntegral $ ((lIMIT - s) * 255) `quot` lIMIT
    r = (t `mod` 128 + 64)
    g = (t * 2 `mod` 128 + 64) * 0x100
    b = (t * 3 `mod` 256) * 0x10000
    a = 0xFF000000

makePicture ∷ Options → Int → Acc (Array DIM2 (Complex, Int)) → R.Array R.F R.DIM3 Word8
makePicture opt limit zs = R.fromForeignPtr (R.Z R.:. h R.:. w R.:. 4) rawData
  where
    arrPixels = run opt $ A.map (prettyRGBA (constant limit)) zs
    (Z :. h :. w) = arrayShape arrPixels

    {-# NOINLINE rawData #-}
    rawData =
      let (Array _ adata) = arrPixels
          ((), ptr) = ptrsOfArrayData adata
       in unsafePerformIO $ newForeignPtr_ (castPtr ptr)

main ∷ IO ()
main =
  do
    (config, nops) ← processArgs =<< getArgs
    let size = optSize config
        limit = optLimit config
        --
        x = -0.25 -- should get this from command line as well
        y = -1.0
        x' = 0.0
        y' = -0.75
        --
        image =
          makePicture config limit $
            mandelbrot x y x' y' size size limit

    runIL $ writeImage "out.png" (RGBA image)
