-- | Module:     Control.Varying.Tween
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
module Control.Varying.Tween where

import Control.Varying.Core
import Control.Varying.Event hiding (after, before)
import Control.Varying.Time
import Control.Arrow

-- | Ease in quadratic.
easeInQuad :: Num t => Easing t
easeInQuad c t b =  c * t*t + b

-- | Ease out quadratic.
easeOutQuad :: Num t => Easing t
easeOutQuad c t b =  (-c) * (t * (t - 2)) + b

-- | Ease in and out quadratic.
easeInOutQuad :: (Ord t, Fractional t) => Easing t
easeInOutQuad = easeInOut easeInQuad easeOutQuad

-- | Ease in cubic.
easeInCubic :: Num t => Easing t
easeInCubic c t b =  c * t*t*t + b

-- | Ease out cubic.
easeOutCubic :: Num t => Easing t
easeOutCubic c t b =  let t' = t - 1 in c * (t'*t'*t' + 1) + b

-- | Ease in and out cubic.
easeInOutCubic :: (Ord t, Fractional t) => Easing t
easeInOutCubic = easeInOut easeInCubic easeOutCubic

-- | Ease in by some power.
easeInPow :: Num t => Int -> Easing t
easeInPow power c t b =  c * (t^power) + b

-- | Ease out by some power.
easeOutPow :: Num t => Int -> Easing t
easeOutPow power c t b =
    let t' = t - 1
        c' = if power `mod` 2 == 1 then c else -c
        i  = if power `mod` 2 == 1 then 1 else -1
    in c' * ((t'^power) + i) + b

-- | Ease in sinusoidal.
easeInSine :: Floating t => Easing t
easeInSine c t b =  let cos' = cos (t * (pi / 2))
                               in -c * cos' + c + b

-- | Ease out sinusoidal.
easeOutSine :: Floating t => Easing t
easeOutSine c t b =  let cos' = cos (t * (pi / 2)) in c * cos' + b

-- | Ease in and out sinusoidal.
easeInOutSine :: Floating t => Easing t
easeInOutSine c t b =  let cos' = cos (pi * t)
                                  in (-c / 2) * (cos' - 1) + b

-- | Ease in exponential.
easeInExpo :: Floating t => Easing t
easeInExpo c t b =  let e = 10 * (t - 1) in c * (2**e) + b

-- | Ease out exponential.
easeOutExpo :: Floating t => Easing t
easeOutExpo c t b =  let e = -10 * t in c * (-(2**e) + 1) + b

-- | Ease in and out exponential.
easeInOutExpo :: (Ord t, Floating t) => Easing t
easeInOutExpo = easeInOut easeInExpo easeOutExpo

-- | Ease in circular.
easeInCirc :: Floating t => Easing t
easeInCirc c t b = let s = sqrt (1 - t*t) in -c * (s - 1) + b

-- | Ease out circular.
easeOutCirc :: Floating t => Easing t
easeOutCirc c t b = let t' = (t - 1)
                        s  = sqrt (1 - t'*t')
                    in c * s + b

-- | Ease in and out circular.
easeInOutCirc :: (Ord t, Floating t) => Easing t
easeInOutCirc = easeInOut easeInCirc easeOutCirc

-- | Ease in and out using the given easing equations.
easeInOut :: (Ord t, Num t, Fractional t) => Easing t -> Easing t -> Easing t
easeInOut ein eout c t b = if t >= 0.5 then ein c t b else eout c t b

-- | Ease linear.
linear :: Num t => Easing t
linear c t b = c * t + b

-- | Ease none.
-- This performs no interpolation over the duration, it just samples at a
-- constant value until the duration is up.
constant :: (Monad m, Num t, Ord t) => a -> t -> Var m t (Event a)
constant value duration = use value $ before duration

-- | An easing function.
type Easing t = t -> t -> t -> t

-- | A linear interpolation between two values over some duration.
type Tween m t = t -> t -> t -> Var m t (Event t)

-- | Produces an event sample interpolated between a start and end value
-- using an easing equation ('Easing') over a duration. The resulting 'Var' will
-- take a time delta as input. For example:
--
-- @
-- testWhile_ isEvent v
--    where v :: Var IO a (Event Double)
--          v = deltaUTC ~> tween easeOutExpo 0 100 5
-- @
--
-- Keep in mind `tween` must be fed time deltas, not absolute time or
-- duration. This is mentioned because the author has made that mistake
-- more than once ;)
tween :: (Monad m, Fractional t, Ord t)
      => Easing t -> t -> t -> t -> Var m t (Event t)
tween f start end dur = proc dt -> do
    -- Current time as percentage / amount of interpolation (0.0 - 1.0)
    t <- timeAsPercentageOf dur -< dt
    -- Emitted event
    e <- before dur -< dt
    -- Total change in value
    let c = end - start
        b = start
        x = f c t b
    -- Tag the event with the value.
    returnA -< x <$ e

-- | Varies 0.0 to 1.0 linearly for duration `t` and 1.0 after `t`.
timeAsPercentageOf :: (Monad m, Ord t, Num t, Fractional t) => t -> Var m t t
timeAsPercentageOf t = proc dt -> do
    t' <- accumulate (+) 0 -< dt
    returnA -< min 1 (t' / t)
