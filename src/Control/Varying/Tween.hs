-- |
--   Module:     Control.Varying.Tween
--   Copyright:  (c) 2016 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell@takt.com>
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

{-# LANGUAGE Rank2Types   #-}
module Control.Varying.Tween
  ( -- * Tweening types
    Easing
  , TweenT
  , Tween
    -- * Running tweens
  , runTweenT
  , scanTween
  , tweenStream
    -- * Creating tweens
    -- $creation
  , tween
  , tween_
  , constant
  , withTween
  , withTween_
    -- * Interpolation functions
    -- $lerping
  , linear
  , easeInCirc
  , easeOutCirc
  , easeInExpo
  , easeOutExpo
  , easeInSine
  , easeOutSine
  , easeInOutSine
  , easeInPow
  , easeOutPow
  , easeInCubic
  , easeOutCubic
  , easeInQuad
  , easeOutQuad
    -- * Writing your own tweens
    -- $writing
  ) where

import           Control.Monad             (void)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put,
                                            runStateT)
import           Control.Varying.Core      (VarT (..), done)
import           Control.Varying.Spline    (SplineT (..), mapOutput, scanSpline,
                                            untilEvent_)
import           Control.Varying.Event     (after)
import           Data.Functor.Identity     (Identity)

--------------------------------------------------------------------------------
-- $lerping
-- These pure functions take a `c` (total change in value, ie end - start),
-- `t` (percent of duration completion) and `b` (start value) and result in
-- an interpolation of a value. To see what these look like please check
-- out http://www.gizma.com/easing/.
--------------------------------------------------------------------------------
-- | Ease in quadratic.
easeInQuad :: (Fractional t, Real f) => Easing t f
easeInQuad c t b =  c * realToFrac (t*t) + b

-- | Ease out quadratic.
easeOutQuad :: (Fractional t, Real f) => Easing t f
easeOutQuad c t b =  (-c) * realToFrac (t * (t - 2)) + b

-- | Ease in cubic.
easeInCubic :: (Fractional t, Real f) => Easing t f
easeInCubic c t b =  c * realToFrac (t*t*t) + b

-- | Ease out cubic.
easeOutCubic :: (Fractional t, Real f) => Easing t f
easeOutCubic c t b =  let t' = realToFrac t - 1 in c * (t'*t'*t' + 1) + b

-- | Ease in by some power.
easeInPow :: (Fractional t, Real f) => Int -> Easing t f
easeInPow power c t b =  c * (realToFrac t^power) + b

-- | Ease out by some power.
easeOutPow :: (Fractional t, Real f) => Int -> Easing t f
easeOutPow power c t b =
    let t' = realToFrac t - 1
        c' = if power `mod` 2 == 1 then c else -c
        i  = if power `mod` 2 == 1 then 1 else -1
    in c' * ((t'^power) + i) + b

-- | Ease in sinusoidal.
easeInSine :: (Floating t, Real f) => Easing t f
easeInSine c t b =  let cos' = cos (realToFrac t * (pi / 2))
                               in -c * cos' + c + b

-- | Ease out sinusoidal.
easeOutSine :: (Floating t, Real f) => Easing t f
easeOutSine c t b =  let cos' = cos (realToFrac t * (pi / 2)) in c * cos' + b

-- | Ease in and out sinusoidal.
easeInOutSine :: (Floating t, Real f) => Easing t f
easeInOutSine c t b =  let cos' = cos (pi * realToFrac t)
                                  in (-c / 2) * (cos' - 1) + b

-- | Ease in exponential.
easeInExpo :: (Floating t, Real f) => Easing t f
easeInExpo c t b =  let e = 10 * (realToFrac t - 1) in c * (2**e) + b

-- | Ease out exponential.
easeOutExpo :: (Floating t, Real f) => Easing t f
easeOutExpo c t b =  let e = -10 * realToFrac t in c * (-(2**e) + 1) + b

-- | Ease in circular.
easeInCirc :: (Floating t, Real f, Floating f) => Easing t f
easeInCirc c t b = let s = realToFrac $ sqrt (1 - t*t) in -c * (s - 1) + b

-- | Ease out circular.
easeOutCirc :: (Floating t, Real f) => Easing t f
easeOutCirc c t b = let t' = (realToFrac t - 1)
                        s  = sqrt (1 - t'*t')
                    in c * s + b

-- | Ease linear.
linear :: (Floating t, Real f) => Easing t f
linear c t b = c * realToFrac t + b

-- TODO: Don't use StateT for leftover time in Tweens.
-- This creates a funky state where running two tween splines together
-- causes leftover time interplay. Think about continuations or something.

type TweenT f t m = SplineT f t (StateT f m)
type Tween f t = TweenT f t Identity

runTweenT :: TweenT f t m x -> f -> f -> m (Either x (t, TweenT f t m x), f)
runTweenT s dt = runStateT (runSplineT s dt)

scanTween :: (Monad m, Num f)
          => TweenT f t m a -> t -> [f] -> m [t]
scanTween s t dts = evalStateT (scanSpline s t dts) 0

-- | Converts a tween into a continuous value stream. This is the tween version
-- of `outputStream`.
tweenStream :: (Monad m, Num f)
            => TweenT f t m x -> t -> VarT m f t
tweenStream s0 t0 = VarT $ f s0 t0 0
  where f s t l i = do (e, l1) <- runTweenT s i l
                       case e of
                         Left _        -> return (t, done t)
                         Right (b, s1) -> return (b, VarT $ f s1 b l1)

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
-- resulting spline will take a time delta as input.
-- Keep in mind that `tween` must be fed time deltas, not absolute time or
-- duration. This is mentioned because the author has made that mistake
-- more than once ;)
--
-- `tween` concludes returning the latest output value.
tween :: (Monad m, Real f, Fractional f, Real t)
      => Easing t f -> t -> t -> f -> TweenT f t m t
tween f start end dur = SplineT g
  where c = end - start
        b = start
        g dt = do
          leftover <- get
          let t = dt + leftover

          if t == dur
            then do put 0
                    return $ Right (end, return end)
            else if t > dur
              then do put $ t - dur - dt
                      return $ Left end
              else do put t
                      return $ Right (f c (t/dur) b, SplineT g)

-- | A version of 'tween' that discards the result. It is simply
--
-- @
-- tween f a b c >> return ()
-- @
--
tween_ :: (Monad m, Real t, Real f, Fractional f)
       => Easing t f -> t -> t -> f -> TweenT f t m ()
tween_ f a b c = Control.Monad.void (tween f a b c)

-- | A version of 'tween' that maps its output using the given constant
-- function.
-- @
-- withTween ease from to dur f = mapOutput (pure f) $ tween ease from to dur
-- @
withTween :: (Monad m, Real t, Real a, Fractional a)
          => Easing t a -> t -> t -> a -> (t -> x) -> TweenT a x m t
withTween ease from to dur f = mapOutput (pure f) $ tween ease from to dur

-- | A version of 'withTween' that discards its output.
withTween_ :: (Monad m, Real t, Real a, Fractional a)
           => Easing t a -> t -> t -> a -> (t -> x) -> TweenT a x m ()
withTween_ ease from to dur f = Control.Monad.void (withTween ease from to dur f)

-- | Creates a tween that performs no interpolation over the duration.
constant :: (Monad m, Num t, Ord t)
         => a -> t -> TweenT t a m a
constant value duration = pure value `untilEvent_` after duration
--------------------------------------------------------------------------------
-- $writing
-- To create your own tweens just write a function that takes a start
-- value, end value and a duration and return an event stream.
--
-- @
-- tweenInOutExpo start end dur = do
--     (dt, x) <- tween easeInExpo start end (dur/2)
--     tween easeOutExpo x end $ dt + dur/2
-- @
--------------------------------------------------------------------------------
-- | An easing function. The parameters are often named `c`, `t` and `b`,
-- where `c` is the total change in value over the complete duration
-- (endValue - startValue), `t` is the current percentage (0 to 1) of the
-- duration that has elapsed and `b` is the start value.
--
-- To make things simple only numerical values can be tweened and the type
-- of time deltas must match the tween's value type. This may change in the
-- future :)
type Easing t f = t -> f -> t -> t
