-- | Module:     Control.Varying.Time
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
module Control.Varying.Time where

import Control.Varying.Core
import Control.Varying.Event
import Control.Applicative
--------------------------------------------------------------------------------
-- Using timed events
--------------------------------------------------------------------------------
-- | Emits events before accumulating t of input dt.
-- Note that as soon as we have accumulated >= t we stop emitting events
-- and therefore an event will never be emitted exactly at time == t.
before :: (Applicative m, Monad m, Num t, Ord t) => t -> VarT m t (Event t)
before t = accumulate (+) 0 >>> onWhen (< t)

-- | Emits events after t input has been accumulated.
-- Note that event emission is not guaranteed to begin exactly at t,
-- since it depends on the input.
after :: (Applicative m, Monad m, Num t, Ord t) => t -> VarT m t (Event t)
after t = accumulate (+) 0 >>> onWhen (>= t)
