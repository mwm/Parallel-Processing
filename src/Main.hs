-- | Main entry point to the application.
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, TupleSections #-}
module Main where

{- This module uses the Yesod framework to provide a web interface for invoking
the two evalutors defined in Evalutors.

Before starting Yesod, it allocates two cores with setNumCapabilities.

The two different evaluators are then run in doTimes to collect timeing information
from getProcessTimes, and report on the number of CPUs allocated with
getNumCapabilities.
-}
    
import Yesod
import System.Environment
import System.Posix.Process
import Control.DeepSeq
import GHC.Conc


import Evaluators

-- Web server portion
data Mandelbrot = Mandelbrot {iterations :: Int}

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

calcDiffs a b = ["CPU Seconds: " ++ show cpus,
                 "Elapsed seconds: " ++ show elapsed,
                 "Ratio: " ++ show ratio]
    where cpus = getDiff userTime
          elapsed = getDiff elapsedTime
          ratio = cpus / elapsed
          getDiff mem = (fromIntegral . fromEnum $ mem a - mem b) / 100

doTime a = do
    master <- getYesod
    let its = iterations master
    start <- liftIO getProcessTimes
    let val = a its
    -- use deepseq here to force the evaluation of val before we get the new times.
    stop <- liftIO $ val `deepseq` getProcessTimes
    cs <- liftIO getNumCapabilities
    let res = val ++ ["CPUS: " ++ show cs] ++ calcDiffs stop start
    defaultLayout [whamlet|<a href=@{HomeR}>Home</a><pre>#{unlines res}|]
    

getUnthreadedR = doTime single
getThreadedR = doTime multi
main = do
        args <- liftIO getArgs
        setNumCapabilities 2
        warpEnv $ Mandelbrot 2000

