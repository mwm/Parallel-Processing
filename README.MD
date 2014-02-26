# Haskell makes parallel processing easy.

Writing parallel code in imperative languages - Java, C++, C#, Python, Ruby, Perl, etc. -
is a difficult task. You generally have two choices: use low-level tools that are inspired
by the Posix threading API, figuring out when data is shared, how it has to be locked,
and locking it correctly. The result is often code littered with heisenbugs that depend on
timing relationships between the various threads. Alternatively, there are high-level tools
that create queues that let you pass data between threads safely, queueing work and otherwise
making your life simpler. Except that in modern languages, it's never clear what data values
might be shared, so you once again wind up having to figure out what needs be locked and how it
needs to locked. While better than the alternative, it still tends to create bugs that are
hard to reproduce, find and fix.

To add insult to injury, if you develop the code in a single-threaded environment, converting
it to run in parallel is often non-trivial, involving refactoring things to create code segments
that can be run in parallel, and then creating a module that arranges to run each of those bits
appropriately and collect the results.

Haskell solves - or can solve, if you go with it - a lot of these problems. For a more complete
coverage of the topic of parallel and concurrent Haskell, see [Parallel and Concurrent Programming
in Haskell](http://chimera.labs.oreilly.com/books/1230000000929)

## Pure code

*Pure* code is code with no side effects. It doesn't change local variables. It doesn't change
global variables. It doesn't do IO. This means that the output value of a function depends on
the arguments and nothing but the arguments. No matter how many times you call a pure function
with the same argument, you'll always get the same value returned.

By structuring your code so that the computations are done in pure code, you can eliminate all
the issues of needing locks during the parallel computation. Since nothing is modified, there's
no need to lock anything. This kind of structure is actually straightforward. It amounts to having
an interface section that does IO and is hence impure, and then a computation section that is pure
code that is called after all the IO is done.

This example does that. The `Mandelbrot` module in `Mandelbrot.hs` is pure code. The `Main` module is all
interface code, and does all the IO.

## Parallel segments

In order to run computations in parallel, the parts running in parallel must not depend on each in
any way. If they are intedependent, you'll need some kind of rendezvous mechanism.  This is less
likely to happen in pure code, because you can't wait for values to change. Pure code doesn't
guarantee rendezvous-free parallel code, but it does make it easier.

This example is particularly easy to make parallel. It iterates over the points in a square region
of the plane, determining whether each point is a member of the *mandelbrot* set. This requires
iterating the point through a function to see if it exist the region of the plane, and we then
assign an ASCII graphic to the point depending on the number of iterations that took, or 0 if we gave
up. Since the only values used to check a point are the point itself, each point can be checked in
parallel with all the others.

In this case, we're only going to check the rows in parallel with each other. This speeds up the
process to use almost all of the two cores we allocate, so reaching further into the code to make
the parallel bits finer grained won't improve things.

## The example

`Main.hs` provides the interface for the module. It sets the number of CPUs to use to two, or at
least tries to. Then it runs a simple Yesod web server, with a home page that links to either a
single or multi-threaded run of the mandelbrot example. Running an example yields an ASCII plot
of the Mandelbrot set, which should be familiar, except for using ASCII graphics. It also does
some simple-minded timing and prints the results of that along with the CPU count, just to give you an
idea of how much difference there is between the two.

`Mandelbrot.hs` does the actual calculations - up to the level of the row. I'm not going to discuss it
in detail, as it's pretty straightforward once you have the definition of the set in hand. The final
bit of code is iterating over the set of rows, which is done in `Evaluators.hs`.

`Evaluators.hs` is pulled out of `Mandelbrot.hs`, because there are two versions of the code. One version,
`single` just iterates over each row in the set to produce a list of lines in the set. The second
version, `multi`, does the same, except it does it in parallel so that multiple cores will be leveraged.

Check the comments in `Evaluators.hs` for more information on how this works.

### Running in the IDE

Running this in the IDE takes about 20 seconds with a single thread, and 10 with multiple threads
as is. Both versions have two cores allocated, so it is possible the single-threaded version will
use more CPU time than elapsed time, as the runtime system time is included in the timing, and
some of that may be done on the second CPU. The parallel version usually gets better than 1.9 times
as much CPU time as elapsed time, which isn't bad for adding three words to a single line.

Since the IDE runs unoptimized code, if you're going to build a version to deploy or download, you
should consider changing the number of iterations in Main.hs from 2000 to 10000 or more.
