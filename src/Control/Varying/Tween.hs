-- |
--   Module:     Control.Varying.Tween
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   Tweening is a technique of generating intermediate samples of a type
--   __between__ a start and end value. By sampling a running tween
--   each frame we get a smooth animation of a value over time.
--
--   At first release `varying` is only capable of tweening numerical
--   values of type @(Fractional t, Ord t) => t@ that match the type of
--   time you use. At some point it would be great to be able to tween
--   arbitrary types, and possibly tween one type into another (pipe
--   dreams).

--
{-# LANGUAGE Rank2Types #-}
module Control.Varying.Tween (
    -- * Creating tweens
    -- $creation
    tween,
    constant,
    timeAsPercentageOf,
    -- * Interpolation functions
    -- $lerping
    linear,
    easeInCirc,
    easeOutCirc,
    easeInExpo,
    easeOutExpo,
    easeInSine,
    easeOutSine,
    easeInOutSine,
    easeInPow,
    easeOutPow,
    easeInCubic,
    easeOutCubic,
    easeInQuad,
    easeOutQuad,
    -- * Writing your own tweens
    -- $writing
    Tween,
    Easing
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Varying.Spline
import Control.Varying.Time
import Control.Arrow
import Control.Applicative

--------------------------------------------------------------------------------
-- $lerping
-- These pure functions take a `c` (total change in value, ie end - start),
-- `t` (percent of duration completion) and `b` (start value) and result in
-- and interpolation of a value. To see what these look like please check
-- out http://www.gizma.com/easing/.
--------------------------------------------------------------------------------

-- | Ease in quadratic.
easeInQuad :: Num t => Easing t
easeInQuad c t b =  c * t*t + b

-- | Ease out quadratic.
easeOutQuad :: Num t => Easing t
easeOutQuad c t b =  (-c) * (t * (t - 2)) + b

-- | Ease in cubic.
easeInCubic :: Num t => Easing t
easeInCubic c t b =  c * t*t*t + b

-- | Ease out cubic.
easeOutCubic :: Num t => Easing t
easeOutCubic c t b =  let t' = t - 1 in c * (t'*t'*t' + 1) + b

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

-- | Ease in circular.
easeInCirc :: Floating t => Easing t
easeInCirc c t b = let s = sqrt (1 - t*t) in -c * (s - 1) + b

-- | Ease out circular.
easeOutCirc :: Floating t => Easing t
easeOutCirc c t b = let t' = (t - 1)
                        s  = sqrt (1 - t'*t')
                    in c * s + b

-- | Ease linear.
linear :: Num t => Easing t
linear c t b = c * t + b

--------------------------------------------------------------------------------
-- $creation
-- The most direct route toward tweening values is to use 'tween'
-- along with an interpolation function such as 'easeInExpo'. For example,
-- @tween easeInExpo 0 100 10@, this will create a spline that produces a
-- number interpolated from 0 to 100 over 10 seconds. At the end of the
-- tween the spline will return the result value.
--------------------------------------------------------------------------------

-- | Creates a spline that produces a value interpolated between a start and
-- end value using an easing equation ('Easing') over a duration.  The
-- resulting spline will take a time delta as input. For example:
--
-- @
-- testWhile_ isEvent (deltaUTC >>> v)
--    where v :: VarT IO a (Event Double)
--          v = execSpline 0 $ tween easeOutExpo 0 100 5
-- @
--
-- Keep in mind `tween` must be fed time deltas, not absolute time or
-- duration. This is mentioned because the author has made that mistake
-- more than once ;)
tween :: (Applicative m, Monad m, Fractional t, Ord t)
      => Easing t -> t -> t -> t -> SplineT t t m t
tween f start end dur =
  let c = end - start
      b = start
      vt = h <$> timeAsPercentageOf dur
      h t = if t >= 1.0
            then (end, Event end)
            else (f c t b, NoEvent)
  in SplineT vt


-- | Creates a tween that performs no interpolation over the duration.
constant :: (Applicative m, Monad m, Num t, Ord t)
         => a -> t -> SplineT t a m a
constant value duration = pure value `untilEvent_` after duration

-- | VarTies 0.0 to 1.0 linearly for duration `t` and 1.0 after `t`.
timeAsPercentageOf :: (Applicative m, Monad m, Ord t, Num t, Fractional t)
                   => t -> VarT m t t
timeAsPercentageOf t = (/t) <$> accumulate (+) 0

--------------------------------------------------------------------------------
-- $writing
-- To create your own tweens just write a function that takes a start
-- value, end value and a duration and return an event stream.
--
-- @
-- tweenInOutExpo s e d = execSpline s $ do
--     x <- tween easeInExpo s e (d/2)
--     tween easeOutExpo x e (d/2)
-- @
--------------------------------------------------------------------------------
-- | An easing function. The parameters or often named `c`, `t` and `b`,
-- where `c` is the total change in value over the complete duration
-- (endValue - startValue), `t` is the current percentage of the duration
-- that has elapsed and `b` is the start value.
--
-- To make things simple only numerical values can be tweened and the type
-- of time deltas much match the tween's value type. This may change in the
-- future :)
type Easing t = t -> t -> t -> t

-- | A linear interpolation between two values over some duration.
-- A `Tween` takes three values - a start value, an end value and
-- a duration.
type Tween m t = t -> t -> t -> VarT m t (Event t)
