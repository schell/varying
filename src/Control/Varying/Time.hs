-- | Module:     Control.Varying.Time
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
{-# LANGUAGE TupleSections #-}
module Control.Varying.Time where

import Control.Varying.Core
import Control.Varying.Event
import Control.Applicative
import Data.Time.Clock

-- | Produces time deltas using 'getCurrentTime' and 'diffUTCTime'.
deltaUTC :: Fractional t => Var IO b t
deltaUTC = delta getCurrentTime (\a b -> realToFrac $ diffUTCTime a b)

-- | Produces time deltas using a monadic computation and a difference
-- function.
delta :: (Num t, Fractional t, Applicative m, Monad m)
      => m a -> (a -> a -> t) -> Var m b t
delta m f = Var $ \_ -> do
    t <- m
    return (0, delta' t)
    where delta' t = Var $ \_ -> do
            t' <- m
            let dt = t' `f` t
            return (dt, delta' t')
--------------------------------------------------------------------------------
-- Using timed events
--------------------------------------------------------------------------------
-- | Emits events before accumulating t of input dt.
-- Note that as soon as we have accumulated >= t we stop emitting events
-- and there is no guarantee that an event will be emitted at time == t.
before :: (Applicative m, Monad m, Num t, Ord t) => t -> Var m t (Event ())
before t = Var $ \dt -> return $
    if t - dt >= 0
    then (Event (), before $ t - dt)
    else (NoEvent, never)

-- | Emits events after t input has been accumulated.
-- Note that event emission is not guaranteed to begin exactly at t,
-- only at some small delta after t.
after :: (Applicative m, Monad m, Num t, Ord t) => t -> Var m t (Event ())
after t = Var $ \dt -> return $
    if t - dt <= 0
    then (Event (), pure $ Event ())
    else (NoEvent, after $ t - dt)
