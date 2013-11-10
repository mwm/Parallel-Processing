-- | Main entry point to the application.
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
module Main where

import Yesod
import Control.Monad (mapM_)
import Control.Parallel.Strategies
import Control.DeepSeq

import Control.Monad (mapM_)
import Control.Parallel.Strategies
import Control.DeepSeq

xmin = -2 :: Double
xmax = 1 :: Double
xstep = 80
ymin = -1 :: Double
ymax = 1 :: Double
ystep = 100
iters = 100000

-- row and column calculations (could be cleaner)
xs = map (\xc -> xc*(xmax-xmin)/xstep + xmin) [0..(xstep-1)] :: [Double]
ys = map (\yc -> yc*(ymax-ymin)/ystep + ymin) [0..(ystep-1)] :: [Double]

escapesRec :: Int -> Double -> Double -> Double -> Double -> Bool
escapesRec 0 _ _ _ _ = False
escapesRec iter cr ci zr zi
  | (zr*zr + zi*zi) > 4   = True
  | otherwise             = (escapesRec (iter-1) cr ci $! (zr*zr - zi*zi + cr)) $! (2*zr*zi + ci)

escapes :: Int -> Double -> Double -> Bool
escapes iter cr ci = escapesRec iter cr ci 0 0

row ci = r `deepseq` r -- force strict evaluation of the list
  where
    r = map toChar [escapes iters cr ci  | cr <- xs]
    toChar b = if b then ' ' else 'X'

-- Web server portion
data Mandelbrot = Mandelbrot

instance Yesod Mandelbrot

mkYesod "Mandelbrot" [parseRoutes|
  / HomeR GET
  /unthreaded UnthreadedR GET
  /threaded ThreadedR GET
|]

getHomeR = defaultLayout [whamlet|
Try the
<a href=@{UnthreadedR}>single-threaded
or
<a href=@{ThreadedR}>multi-threaded
versions.
|]

getUnthreadedR = defaultLayout [whamlet|#{unlines $ map row ys}|]
getThreadedR = defaultLayout [whamlet|#{unlines $ parMap rseq row ys}|]

main = warpEnv Mandelbrot