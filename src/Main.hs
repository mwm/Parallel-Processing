module Main where

{- This module uses a short MFlow flow to allow the user to select between the two evaluators.
   It then runs the evaluator with the doTime wrapper to provide some simple timing information
   for comparison. -}
    
import MFlow.Wai.Blaze.Html.All
import System.Environment
import System.Posix.Process
import Control.DeepSeq
import GHC.Conc

import Evaluators

main = do
    liftIO $ setNumCapabilities 2
    runNavigation "" . step $ do
        doMulti <- page  $   h3 << "Parallel processing example"
               ++> toHtml << "Run the "
               ++> wlink True  << toHtml << "multi-threaded"
               <++ toHtml << " or "
               <|> wlink False << toHtml << "single-threaded"
               <++ toHtml << " versions. Please allow them a minute or so to run."

        page $ do
            lines <- liftIO $ doTime (if doMulti then multi else single)
            h3 << "ASCII Mandelbrot" ++> pre << lines ++> wlink () << toHtml << "Home"

-- doTime runs one of the two evaluators in a wrapper that gets rough cpu and elapsed times so they
-- can be compared, then outputs the set along with the timing information in one string
doTime a = do
    start <- getProcessTimes
    let val = a 2000
    -- use deepseq here to force the evaluation of val before we get the new times.
    stop <- val `deepseq` getProcessTimes
    cs <- liftIO getNumCapabilities
    return $ unlines val ++ "\nCPUS: " ++ show cs ++ calcDiffs stop start

-- calcDiffs uses the process times to calculate elapsed time, used cpu time, and their ratio and return
-- a string providing that information
calcDiffs a b = "\nCPU Seconds: " ++ show cpus ++
                "\nElapsed seconds: " ++ show elapsed ++
                "\nRatio: " ++ show ratio
    where cpus = getDiff userTime
          elapsed = getDiff elapsedTime
          ratio = cpus / elapsed
          getDiff mem = (fromIntegral . fromEnum $ mem a - mem b) / 100

