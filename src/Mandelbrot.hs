module Mandelbrot where

{- This is the code that actually calculates the values that go into the final image.
It builds a low-resolution image of the Mandelbrot set. While it's not very pretty,
you ought to at least recognize the shape when it's done.

The one special thing about this code is that it is all pure. The value of a function
depends on it's arguments, and nothing else. This allows the computation of the rows to
be done parallel, since the values from one computation don't depend on the values being
used in the computation of another row.

If you don't see anything special about this code - you're right. Writing pure code is
the standard idiom for doing math in Haskell.
-}


xmin = -2 :: Double
xmax = 1 :: Double
xstep = 120
ymin = -1 :: Double
ymax = 1 :: Double
ystep = 60

-- row and column calculations (could be cleaner)
xs = map (\xc -> xc*(xmax-xmin)/xstep + xmin) [0..(xstep-1)] :: [Double]
ys = map (\yc -> yc*(ymax-ymin)/ystep + ymin) [0..(ystep-1)] :: [Double]

escapesRec :: Int -> Double -> Double -> Double -> Double -> Int
escapesRec 0 _ _ _ _ = 0
escapesRec iter cr ci zr zi
  | (zr*zr + zi*zi) > 4   = iter
  | otherwise             = (escapesRec (iter-1) cr ci $! (zr*zr - zi*zi + cr)) $! (2*zr*zi + ci)

escapes :: Int -> Double -> Double -> Int
escapes iter cr ci = escapesRec iter cr ci 0 0

row iters ci = map toChar [escapes iters cr ci  | cr <- xs]
  where toChar b = "@O*+ " !! (b `rem` 5)
