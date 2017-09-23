-- |
--  Module:     Control.Varying
--  Copyright:  (c) 2015 Schell Scivally
--  License:    MIT
--  Maintainer: Schell Scivally <schell@takt.com>
--
--  [@Core@]
--  Get started writing value streams using the pure constructor 'var', the
--  monadic constructor 'varM' or the raw constructor 'VarT'
--
--  [@Event@]
--  Write event streams using the many event emitters and combinators.
--
--  [@Spline@]
--  Use do-notation to sequence event streams to form complex behavior.
--
--  [@Tween@]
--  Tween numerical values over time using interpolation functions and the
--  "quick 'n dirty" time generators in 'Control.Varying.Time'.
--
--  [@Time@]
--  Create time streams and temporal event streams.
--
module Control.Varying (
  -- * Reexports
  module V
) where

import           Control.Varying.Core   as V
import           Control.Varying.Event  as V
import           Control.Varying.Spline as V
import           Control.Varying.Time   as V
import           Control.Varying.Tween  as V
