-- |
--   Module:     Control.Varying.Event
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  'Event' streams describe things that happen at a specific domain.
--  For example, you can think of the event stream
--  @Var IO Double (Event ())@ as an occurrence of `()` at a specific input
--  (`Double`).
--
--  For sequencing streams please check out 'Control.Varying.Spline' which
--  lets you chain together sequences of streams using do-notation.
module Control.Varying.Event (
    Event(..),
    -- * Transforming event values.
    toMaybe,
    isEvent,
    -- * Combining event and value streams
    orE,
    -- * Generating events from value streams
    use,
    onTrue,
    onJust,
    onUnique,
    onWhen,
    -- * Folding event streams
    foldStream,
    -- * List-like operations on event streams
    filterE,
    takeE,
    dropE,
    -- * Creating event streams from values
    once,
    always,
    never,
    -- * Switching
    -- $switching
    switchByMode,
    -- * Bubbling
    onlyWhen,
    onlyWhenE,
) where

import Prelude hiding (until)
import Control.Varying.Core
import Control.Applicative
import Control.Monad
import Data.Monoid
--------------------------------------------------------------------------------
-- Transforming event values into usable values.
--------------------------------------------------------------------------------
-- | Turns an 'Event' into a 'Maybe'.
toMaybe :: Event a -> Maybe a
toMaybe (Event a) = Just a
toMaybe _ = Nothing

-- | Returns 'True' when the 'Event' contains a sample and 'False'
-- otherwise.
isEvent :: Event a -> Bool
isEvent (Event _) = True
isEvent _ = False
--------------------------------------------------------------------------------
-- Combining value streams and events
--------------------------------------------------------------------------------
-- | Produces values from the first unless the second produces event
-- values and if so, produces the values of those events.
orE :: (Applicative m, Monad m) => Var m a b -> Var m a (Event b) -> Var m a b
orE y ye = Var $ \a -> do
    (b, y')  <- runVar y a
    (e, ye') <- runVar ye a
    return $ case e of
        NoEvent  -> (b, orE y' ye')
        Event b' -> (b', orE y' ye')
--------------------------------------------------------------------------------
-- Generating events from values
--------------------------------------------------------------------------------
-- | Populates a varying Event with a value. This is meant to be used with
-- the various 'on...' event triggers. For example
-- @
-- use 1 onTrue
-- @
-- produces values of `Event 1` when the input value is `True`.
use :: (Functor f, Functor e) => a -> f (e b) -> f (e a)
use a v = (a <$) <$> v

-- | Triggers an `Event ()` when the input value is True.
onTrue :: (Applicative m, Monad m) => Var m Bool (Event ())
onTrue = var $ \b -> if b then Event () else NoEvent

-- | Triggers an `Event a` when the input is `Just a`.
onJust :: (Applicative m, Monad m) => Var m (Maybe a) (Event a)
onJust = var $ \ma -> case ma of
                               Nothing -> NoEvent
                               Just a  -> Event a

-- | Triggers an `Event a` when the input is a unique value.
onUnique :: (Applicative m, Monad m, Eq a) => Var m a (Event a)
onUnique = Var $ \a -> return (Event a, trigger a)
    where trigger a' = Var $ \a'' -> let e = if a' == a''
                                             then NoEvent
                                             else Event a''
                                   in return (e, trigger a'')

-- | Triggers an `Event a` when the condition is met.
onWhen :: Applicative m => (a -> Bool) -> Var m a (Event a)
onWhen f = var $ \a -> if f a then Event a else NoEvent
--------------------------------------------------------------------------------
-- Collecting
--------------------------------------------------------------------------------
-- | Like a left fold over all the stream's produced values.
foldStream :: Monad m => (a -> t -> a) -> a -> Var m (Event t) a
foldStream f acc = Var $ \e ->
    case e of
        Event a -> let acc' = f acc a
                   in return (acc', foldStream f acc')
        NoEvent -> return (acc, foldStream f acc)

-- | Produces the given value until the input events produce a value, then
-- produce that value until a new input event produces. This always holds
-- the last produced value, starting with the given value.
-- @
-- time ~> after 3 ~> startingWith 0
-- @
-- This is similar to 'hold' except that it streams events from its input value
-- instead of another var.
startingWith, startWith :: (Applicative m, Monad m) => a -> Var m (Event a) a
startingWith = startWith
startWith a = foldStream (\_ b -> b) a

-- | Produce the given value once and then inhibit forever.
once :: (Applicative m, Monad m) => b -> Var m a (Event b)
once b = Var $ \_ -> return (Event b, never)

-- | Stream through some number of successful events and then inhibit forever.
takeE :: (Applicative m, Monad m)
      => Int -> Var m a (Event b) -> Var m a (Event b)
takeE 0 _ = never
takeE n ve = Var $ \a -> do
    (eb, ve') <- runVar ve a
    case eb of
        NoEvent -> return (NoEvent, takeE n ve')
        Event b -> return (Event b, takeE (n-1) ve')

-- | Inhibit the first n occurences of an event.
dropE :: (Applicative m, Monad m)
      => Int -> Var m a (Event b) -> Var m a (Event b)
dropE 0 ve = ve
dropE n ve = Var $ \a -> do
    (eb, ve') <- runVar ve a
    case eb of
        NoEvent -> return (NoEvent, dropE n ve')
        Event _ -> return (NoEvent, dropE (n-1) ve')

-- | Inhibit all events that don't pass the predicate.
filterE :: (Applicative m, Monad m)
        => (b -> Bool) -> Var m a (Event b) -> Var m a (Event b)
filterE p v = v ~> var check
    where check (Event b) = if p b then Event b else NoEvent
          check _ = NoEvent

-- | Never produces any event values.
never :: (Applicative m, Monad m) => Var m b (Event c)
never = pure NoEvent

-- | Produces events with the initial value forever.
always :: (Applicative m, Monad m) => b -> Var m a (Event b)
always = pure . Event
--------------------------------------------------------------------------------
-- Switching
--------------------------------------------------------------------------------
-- | Switches using a mode signal. Streams maintain state only for the duration
-- of the mode.
switchByMode :: (Applicative m, Monad m, Eq b)
             => Var m a b -> (b -> Var m a c) -> Var m a c
switchByMode switch f = Var $ \a -> do
    (b, _) <- runVar switch a
    (_, v) <- runVar (f b) a
    runVar (switchOnUnique v $ switch ~> onUnique) a
        where switchOnUnique v sv = Var $ \a -> do
                  (eb, sv') <- runVar sv a
                  (c', v')  <- runVar (vOf eb) a
                  return (c', switchOnUnique v' sv')
                      where vOf eb = case eb of
                                         NoEvent -> v
                                         Event b -> f b
--------------------------------------------------------------------------------
-- Bubbling
--------------------------------------------------------------------------------
-- | Produce events of a value stream 'v' only when its input value passes a
-- predicate 'f'.
-- 'v' maintains state while cold.
onlyWhen :: (Applicative m, Monad m)
         => Var m a b -- ^ 'v' - The value stream
         -> (a -> Bool) -- ^ 'f' - The predicate to run on 'v''s input values.
         -> Var m a (Event b)
onlyWhen v f = v `onlyWhenE` hot
    where hot = var id ~> onWhen f

-- | Produce events of a value stream 'v' only when an event stream 'h'
-- produces an event.
-- 'v' and 'h' maintain state while cold.
onlyWhenE :: (Applicative m, Monad m)
          => Var m a b -- ^ 'v' - The value stream
          -> Var m a (Event c) -- ^ 'h' - The event stream
          -> Var m a (Event b)
onlyWhenE v hot = Var $ \a -> do
    (e, hot') <- runVar hot a
    if isEvent e
    then do (b, v') <- runVar v a
            return (Event b, onlyWhenE v' hot')
    else return (NoEvent, onlyWhenE v hot')
--------------------------------------------------------------------------------
-- Event typeclass instances
--------------------------------------------------------------------------------
instance Show a => Show (Event a) where
    show (Event a) = "Event " ++ show a
    show NoEvent   = "NoEvent"

instance (Floating a) => Floating (Event a) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

instance (Fractional a) => Fractional (Event a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational

instance Num a => Num (Event a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance MonadPlus Event

instance Monad Event where
   return = Event
   (Event a) >>= f = f a
   _ >>= _ = NoEvent

instance Alternative Event where
    empty = NoEvent
    (<|>) (Event e) _ = Event e
    (<|>) NoEvent e = e

instance Applicative Event where
    pure = Event
    (<*>) (Event f) (Event a) = Event $ f a
    (<*>) _ _ = NoEvent

-- | Any event is a monoid that responds to mempty with NoEvent. It
-- responds to mappend by always choosing the rightmost event. This means
-- left events are replaced unless the right event is NoEvent.
instance Monoid (Event a) where
    mempty = NoEvent
    mappend a NoEvent = a
    mappend _ b = b

instance Functor Event where
    fmap f (Event a) = Event $ f a
    fmap _ NoEvent = NoEvent

-- | For all intents and purposes you can think of an Event as a Maybe.
-- A value of @Event ()@ means that an event has occurred and that the
-- result is a @()@. A value of @NoEvent@ means that an event did not
-- occur.
--
-- Event streams (like @Var m a (Event b)@) describe events that may occur over
-- varying @a@ (also known as the series of @a@). Usually @a@ would be some
-- form of time or some user input type.
data Event a = Event a | NoEvent deriving (Eq)
