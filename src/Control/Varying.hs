-- |
--  Module:     Control.Varying
--  Copyright:  (c) 2016 Schell Scivally
--  License:    MIT
--  Maintainer: Schell Scivally <schell@takt.com>
--
--  [@Core@]
--  Automaton based value streams.
--
--  [@Event@]
--  Discontinuous value streams that occur only sometimes.
--
--  [@Spline@]
--  Sequencing of value and event streams using do-notation to form complex
--  behavior.
--
--  [@Tween@]
--  Tween numerical values over time using common easing functions. Great for
--  animation.
--
module Control.Varying (
  -- * Reexports
  module V
) where

import           Control.Varying.Core   as V
import           Control.Varying.Event  as V
import           Control.Varying.Spline as V
import           Control.Varying.Tween  as V

-- TODO: CICD and auto-push master to hackage.
-- This would be very nice.
