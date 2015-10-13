# varying
[![Hackage](https://img.shields.io/hackage/v/varying.svg)](http://hackage.haskell.org/package/varying)
[![Build Status](https://travis-ci.org/schell/varying.svg)](https://travis-ci.org/schell/varying)

This library provides automaton based value streams useful for both functional
reactive programming (FRP) and locally stateful programming (LSP). It is 
influenced by the [netwire](http://hackage.haskell.org/package/netwire) and 
[auto](http://hackage.haskell.org/package/auto) packages. Unlike netwire the 
concepts of inhibition and time are explicit (through `Control.Varying.Event` 
and `Control.Varying.Time`). The library aims at being minimal and well 
documented with a small API.

## Getting started

```haskell
module Main where

import Control.Varying
import Control.Varying.Time -- time is not auto-exported
import Control.Applicative
import Text.Printf

-- | A simple 2d point type.
data Point = Point { px :: Float
                   , py :: Float
                   } deriving (Show, Eq)

-- An exponential tween back and forth from 0 to 100 over 2 seconds that
-- loops forever. This spline takes float values of delta time as input,
-- outputs the current x value at every step and would result in () if it
-- terminated.
tweenx :: (Applicative m, Monad m) => Spline Float Float m ()
tweenx = do
    -- Tween from 0 to 100 over 1 second
    x <- tweenTo easeOutExpo 0 100 1
    -- Chain another tween back to the starting position
    _ <- tweenTo easeOutExpo x 0 1
    -- Loop forever
    tweenx

-- A quadratic tween back and forth from 0 to 100 over 2 seconds that never
-- ends.
tweeny :: (Applicative m, Monad m) => Spline Float Float m ()
tweeny = do
    y <- tweenTo easeOutQuad 0 100 1
    _ <- tweenTo easeOutQuad y 0 1
    tweeny

-- Our time signal that provides delta time samples.
time :: Var IO a Float
time = deltaUTC

-- | Our Point value that varies over time continuously in x and y.
backAndForth :: Var IO a Point
backAndForth =
    -- Turn our splines back into continuous value streams. We must provide
    -- a starting value since splines are not guaranteed to be defined at
    -- their edges.
    let x = execSpline 0 tweenx
        y = execSpline 0 tweeny
    in
    -- Construct a varying Point that takes time as an input.
    (Point <$> x <*> y)
        -- Stream in a time signal using the 'plug left' combinator.
        -- We could similarly use the 'plug right' (~>) function
        -- and put the time signal before the construction above. This is needed
        -- because the tween streams take time as an input.
        <~ time

main :: IO ()
main = do
    putStrLn "Varying Example"
    loop backAndForth
        where loop :: Var IO () Point -> IO ()
              loop v = do (point, vNext) <- runVar v ()
                          printf "\nPoint %03.1f %03.1f" (px point) (py point)
                          loop vNext
```
