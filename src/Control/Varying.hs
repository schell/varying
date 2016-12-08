-- |
--  Module:     Control.Varying
--  Copyright:  (c) 2016 Schell Scivally
--  License:    MIT
--  Maintainer: Schell Scivally <efsubenovex@gmail.com>
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
module Control.Varying
  ( -- * Reexports
    module Control.Varying.Core
  , module Control.Varying.Event
  , module Control.Varying.Spline
  , module Control.Varying.Tween
  ) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Varying.Tween
import Control.Varying.Spline 
