-- |
--  Module:     Control.Varying
--  Copyright:  (c) 2015 Schell Scivally
--  License:    MIT
--  Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  The simplest, squishiest FRP library around.
--
--  [@Core@]
--  Get started writing varying values (also called streams) using the pure
--  constructor 'var', the monadic constructor 'varM' or the raw constructor
--  'Var'
--
--  [@Event@]
--  Write event streams using the many event emitters and combinators.
--
--  [@Tween@]
--  Tween numerical values over time using interpolation functions and the
--  "quick 'n dirty" time generators in 'Control.Varying.Time'.
--
--  [@Time@]
--  The 'Control.Varying.Time' module is not reexported because some of the
--  functions collide with those in 'Event' - namely 'before' and 'after'.
--  I think this is okay because in my experience most modules will either
--  deal with events based on user input or events based on time, an in
--  rare cases both - but in that case the majority of streams will be of one
--  type making the choice of which module to import qualified an easy one.
--  The time generator 'Control.Varying.Time.deltaUTC' in 'Control.Varying.Time'
--  is practical and based on 'Data.Time.Clock.getCurrentTime'. It's meant
--  to be simple, not optimal.
--
module Control.Varying (
    -- * Reexports
    module Control.Varying.Core,
    module Control.Varying.Event,
    module Control.Varying.Tween
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Varying.Tween
