module Evaluators where

{- This provides two different methods to evaluate the code in Mandelbrot. The first is "single",
which uses a single thread of control. The second is "multi", which uses as many cores as the
runtime will allow.

The only difference between the two is that "multi" adds "`using` parList rdeepseq" to the
expression. This parallizes the computation because the original map - as in single - provides
a lazy result. `using` will process the list with it's second argument, which has the type
"Strategy". In this case, the strategy "parlist rdeepseq" is to evaluate each element of the
list in parallel ("parList"), and each element will be processed completely ("rdeepseq").
This is still a lazy expression, except that now when it gets evaluated, the elements in the
list will be evaluated in parallel instead of sequentially.

Since this is all pure code, multi could have been defined as "single its `using` parList rdeepseq".
-}
    
import Control.Parallel.Strategies

import Mandelbrot


single its = map (row its) ys

multi its = map (row its) ys `using` parList rdeepseq