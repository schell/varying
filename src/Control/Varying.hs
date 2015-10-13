-- |
--  Module:     Control.Varying
--  Copyright:  (c) 2015 Schell Scivally
--  License:    MIT
--  Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  [@Core@]
--  Get started writing value streams using the pure constructor 'var', the
--  monadic constructor 'varM' or the raw constructor 'Var'
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
    module Control.Varying.Core,
    module Control.Varying.Event,
    module Control.Varying.Spline,
    module Control.Varying.Time,
    module Control.Varying.Tween,
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Varying.Tween
import Control.Varying.Time
import Control.Varying.Spline
