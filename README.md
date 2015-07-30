# varying
This library provides automaton based varying values useful for both functional
reactive programming (FRP) and locally stateful programming (LSP). It is 
influenced by the [netwire](http://hackage.haskell.org/package/netwire) and 
[auto](http://hackage.haskell.org/package/auto) packages. Unlike netwire the 
concepts of inhibition and time are explicit (through `Control.Varying.Event` 
and `Control.Varying.Time`) and the library aims at being minimal and well 
documented with a small API.

Depending on your types and values varying can provide discrete or continuous
time semantics.

## Getting started

```haskell
module Main where

import Control.Varying
import Control.Varying.Time as Time -- time is not auto-exported
import Text.Printf

-- | A simple 2d point type.
data Point = Point { x :: Float
                   , y :: Float
                   } deriving (Show, Eq)

-- | Our Point value that varies over time continuously in x and y.
backAndForth :: Var IO a Point
backAndForth =
    -- Here we use Applicative to construct a varying Point that takes time
    -- as an input.
    (Point <$> tweenx <*> tweeny)
        -- Here we feed the varying Point a time signal using the 'plug left'
        -- function. We could similarly use the 'plug right' (~>) function
        -- and put the time signal before the Point. This is needed because the
        -- tweens take time as an input.
        <~ time

-- An exponential tween back and forth from 0 to 100 over 2 seconds.
tweenx :: Monad m => Var m Float Float
tweenx =
    -- Tweens only happen for a certain duration and so their sample
    -- values have the type (Ord t, Fractional t => Event t). After construction
    -- a tween's full type will be
    -- (Ord t, Fractional t, Monad m) => Var m t (Event t).
     tween easeOutExpo 0 100 1
         -- We can chain another tween back to the starting position using
         -- `andThenE`, which will sample the first tween until it ends and then
         -- switch to sampling the next tween.
         `andThenE`
             -- Tween back to the starting position.
             tween easeOutExpo 100 0 1
                 -- At this point our resulting sample values will still have the
                 -- type (Event Float). The tween as a whole will be an event
                 -- stream. The tween also only runs back and forth once. We'd
                 -- like the tween to loop forever so that our point cycles back
                 -- and forth between 0 and 100 indefinitely.
                 -- We can accomplish this with recursion and the `andThen`
                 -- combinator, which samples an event stream until it
                 -- inhibits and then switches to a normal value stream (a
                 -- varying value). Put succinctly, it disolves our events into
                 -- values.
                 `andThen` tweenx

-- A quadratic tween back and forth from 0 to 100 over 2 seconds.
tweeny :: Monad m => Var m Float Float
tweeny =
    tween easeOutQuad 0 100 1 `andThenE` tween easeOutQuad 100 0 1 `andThen` tweeny

-- Our time signal.
time :: Var IO a Float
time = deltaUTC

main :: IO ()
main = do
    putStrLn "Varying Values"
    loop backAndForth
        where loop :: Var IO () Point -> IO ()
              loop v = do (point, vNext) <- runVar v ()
                          printf "\nPoint %03.1f %03.1f" (x point) (y point)
                          loop vNext
```
