-- |
--   Module:     Control.Varying.Event
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  An event stream is simply a stream of @Maybe a@. This kind of stream is
--  considered to be only defined at those occurances of @Just a@. Events
--  describe things that happen at a specific time, place or any collection of
--  inputs.
--
--  For example, you can think of the event stream
--  @'VarT' 'IO' 'Double' ('Event' ())@ as an occurrence of @()@ at a specific
--  value of 'Double'. It is possible that this 'Double' is time, or it could be
--  the number of ice cream sandwiches eaten by a particular cat.
--
--  In `varying` we use event streams to dynamically update the network while it
--  is running.  For more info on switching and sequencing streams with events
--  please check out 'Control.Varying.Spline', which lets you chain together
--  sequences of values and events using a familiar do-notation.
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800 
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
module Control.Varying.Event
  ( -- * Event constructors (synonyms of Maybe)
    Event
  , event
  , noevent
    -- * Generating events from value streams
  , use
  , onTrue
  , onUnique
  , onWhen
    -- * Folding and gathering event streams
  , foldStream
  , startingWith, startWith
    -- * Combining multiple event streams
  , mergeE
  , anyE
    -- * List-like operations on event streams
  , filterE
  , takeE
  , dropE
    -- * Primitive event streams
  , once
  , always
  , never
    -- * Bubbling
  , onlyWhen
  , onlyWhenE
  ) where

import Prelude hiding (until)
import Control.Varying.Core
import Control.Monad
import Data.Foldable (foldl')

-- stuff for FAMP
#if __GLASGOW_HASKELL__ <= 707
import Control.Applicative
import Data.Function
#endif

type Event = Maybe

-- | A synonym for the @Maybe@ constructor @Just@.
event :: a -> Event a
event = Just

-- | A synonym for the @Maybe@ constructor @Nothing@.
noevent :: Event a
noevent = Nothing
--------------------------------------------------------------------------------
-- Generating events from values
--------------------------------------------------------------------------------
-- |
-- @
-- 'use' :: 'Monad' m => b -> 'VarT' m a ('Event' x) -> 'VarT' m a ('Event' b)
-- @
--
-- Populates a varying Event with a value. This is meant to be used with
-- the various @on...@ event triggers. For example,
-- @
-- 'use' 1 'onTrue'
-- @
-- produces values of @'Event' 1@ when the input value is 'True'.
use :: (Functor f, Functor e) => a -> f (e b) -> f (e a)
use a v = (a <$) <$> v

-- | Triggers an @'Event' ()@ when the input value is 'True'.
--
-- @
-- 'use' b 'onTrue' :: 'Monad' m => 'VarT' m 'Bool' ('Event' b)
-- @
onTrue :: (Applicative m, Monad m) => VarT m Bool (Event ())
onTrue = var $ \b -> if b then Just () else Nothing

-- | Triggers an @'Event' a@ when the input is distinct from the previous
-- input.
--
-- @
-- 'use' b 'onUnique' :: ('Eq' x, 'Monad' m) => 'VarT' m x ('Event' b)
-- @
onUnique :: (Applicative m, Monad m, Eq a) => VarT m a (Event a)
onUnique = VarT $ \a -> return (Just a, trigger a)
    where trigger a' = VarT $ \a'' -> let e = if a' == a''
                                             then Nothing
                                             else Just a''
                                   in return (e, trigger a'')

-- | Triggers an @'Event' a@ when the condition is met.
onWhen :: Applicative m => (a -> Bool) -> VarT m a (Event a)
onWhen f = var $ \a -> if f a then Just a else Nothing
--------------------------------------------------------------------------------
-- Collecting
--------------------------------------------------------------------------------
-- | Like a left fold over all the stream's produced values.
foldStream :: Monad m => (a -> t -> a) -> a -> VarT m (Event t) a
foldStream f acc = VarT $ \e ->
    case e of
      Just  a -> let acc' = f acc a
                 in return (acc', foldStream f acc')
      Nothing -> return (acc, foldStream f acc)

-- | Produces the given value until the input events produce a value, then
-- produce that value until a new input event produces. This always holds
-- the last produced value, starting with the given value.
--
-- @
-- time '>>>' 'Control.Varying.Time.after' 3 '>>>' 'startingWith' 0
-- @
startWith, startingWith :: (Applicative m, Monad m) => a -> VarT m (Event a) a
startWith = foldStream (\_ a -> a)
startingWith = startWith

-- | Stream through some number of successful 'Event's and then inhibit
-- forever.
takeE :: (Applicative m, Monad m)
      => Int -> VarT m a (Event b) -> VarT m a (Event b)
takeE 0 _ = never
takeE n ve = VarT $ \a -> do
    (eb, ve') <- runVarT ve a
    case eb of
        Nothing -> return (Nothing, takeE n ve')
        Just  b -> return (Just b, takeE (n-1) ve')

-- | Inhibit the first n occurences of an 'Event'.
dropE :: (Applicative m, Monad m)
      => Int -> VarT m a (Event b) -> VarT m a (Event b)
dropE 0 ve = ve
dropE n ve = VarT $ \a -> do
    (eb, ve') <- runVarT ve a
    case eb of
        Nothing -> return (Nothing, dropE n ve')
        Just  _ -> return (Nothing, dropE (n-1) ve')

-- | Inhibit all 'Event's that don't pass the predicate.
filterE :: (Applicative m, Monad m)
        => (b -> Bool) -> VarT m a (Event b) -> VarT m a (Event b)
filterE p v = (join . (check <$>)) <$> v
  where check b = if p b then Just b else Nothing
--------------------------------------------------------------------------------
-- Using multiple streams
--------------------------------------------------------------------------------
-- | Combine two 'Event' streams only when both proc at the same time.
mergeE :: (Applicative m, Monad m)
       => (a -> b -> c) -> VarT m a (Event a) -> VarT m a (Event b)
       -> VarT m a (Event c)
mergeE f va vb = (\ea eb -> f <$> ea <*> eb) <$> va <*> vb

-- | Combine two 'Event' streams and produce an 'Event' any time either stream
-- produces. In the case that both streams produce, this produces the 'Event'
-- of the leftmost stream.
anyE :: (Applicative m, Monad m) => [VarT m a (Event b)] -> VarT m a (Event b)
anyE [] = never
anyE vs = VarT $ \a -> do
  outs <- mapM (`runVarT` a) vs
  let f (eb, vs1) (eb1, v) = (msum [eb, eb1], vs1 ++ [v])
  return (anyE <$> foldl' f (Nothing, []) outs)
--------------------------------------------------------------------------------
-- Primitive event streams
--------------------------------------------------------------------------------
-- | Produce the given event value once and then inhibit forever.
once :: (Applicative m, Monad m) => b -> VarT m a (Event b)
once b = VarT $ \_ -> return (Just b, never)

-- | Never produces any 'Event' values.
--
-- @
-- 'never' = 'pure' 'Nothing'
-- @
never :: (Applicative m, Monad m) => VarT m b (Event c)
never = pure Nothing

-- | Produces 'Event's with the initial value forever.
--
-- @
-- 'always' e = 'pure' ('Event' e)
-- @
always :: (Applicative m, Monad m) => b -> VarT m a (Event b)
always = pure . Just
--------------------------------------------------------------------------------
-- Bubbling
--------------------------------------------------------------------------------
-- | Produce events of a stream @v@ only when an event stream @h@ produces an
-- event.
-- @v@ and @h@ maintain state while cold.
onlyWhenE :: (Applicative m, Monad m)
          => VarT m a b -- ^ @v@ - The value stream
          -> VarT m a (Event c) -- ^ @h@ - The event stream
          -> VarT m a (Event b)
onlyWhenE v hot = VarT $ \a -> do
    (e, hot') <- runVarT hot a
    case e of
      Just _ -> do (b, v') <- runVarT v a
                   return (Just b, onlyWhenE v' hot')
      _      ->  return (Nothing, onlyWhenE v hot')

-- | Produce 'Event's of a value stream @v@ only when its input value passes a
-- predicate @f@.
-- @v@ maintains state while cold.
onlyWhen :: (Applicative m, Monad m)
         => VarT m a b -- ^ @v@ - The value stream
         -> (a -> Bool) -- ^ @f@ - The predicate to run on @v@'s input values.
         -> VarT m a (Event b)
onlyWhen v f = v `onlyWhenE` hot
    where hot = var id >>> onWhen f
