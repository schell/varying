{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Control.Varying.Event (
    Event(..),
    -- * Transforming event values.
    toMaybe,
    -- * Combining events and values
    latchWith,
    orE,
    tagOn,
    tagM,
    ringM,
    -- * Generating events from values
    use,
    onTrue,
    onJust,
    onUnique,
    onWhen,
    -- * Using events
    collect,
    hold,
    holdWith,
    startingWith,
    startWith,
    -- * Stream primitives
    between,
    until,
    after,
    beforeWith,
    beforeOne,
    before,
    filterE,
    takeE,
    once,
    always,
    never,
    -- * Switching and chaining events
    andThen,
    andThenWith,
    andThenE,
) where

import Prelude hiding (until)
import Control.Varying.Core
import Control.Applicative
import Control.Arrow
--------------------------------------------------------------------------------
-- Transforming event values into usable values.
--------------------------------------------------------------------------------
toMaybe :: Event a -> Maybe a
toMaybe (Event a) = Just a
toMaybe _ = Nothing
--------------------------------------------------------------------------------
-- Combining varying values and events
--------------------------------------------------------------------------------
-- | Holds the last value of one event stream while waiting for another event
-- stream to produce a value. Once both streams have produced a value combine
-- the two using the given combine function.
latchWith :: Monad m
          => (b -> c -> d) -> Var m a (Event b) -> Var m a (Event c)
          -> Var m a (Event d)
latchWith f vb vc = latchWith' (NoEvent, vb) vc
    where latchWith' (eb, vb') vc' =
              Var $ \a -> do (eb', vb'') <- runVar vb' a
                             (ec', vc'') <- runVar vc' a
                             let eb'' = eb' <|> eb
                             return $ (do f <$> eb'' <*> ec', latchWith' (eb'', vb'') vc'')


-- | Produces values from the first unless the second produces event
-- values and if so, produces the values of those events.
orE :: Monad m => Var m a b -> Var m a (Event b) -> Var m a b
orE y ye = Var $ \a -> do
    (b, y')  <- runVar y a
    (e, ye') <- runVar ye a
    return $ case e of
        NoEvent  -> (b, orE y' ye')
        Event b' -> (b', orE y' ye')

-- | Injects the values of the `vb` into the events of `ve`.
tagOn :: Monad m => Var m a b -> Var m a (Event c) -> Var m a (Event b)
tagOn vb ve = proc a -> do
    b <- vb -< a
    e <- ve -< a
    returnA -< b <$ e

-- | Injects a monadic computation into an event stream, using the event
-- values of type `b` as a parameter to produce an event stream of type
-- `c`. After the first time an event is generated the result of the
-- previous event is used in a clean up function.
--
-- This is like `tagM` but performs a cleanup function first.
ringM :: Monad m
      => (c -> m ()) -> (b -> m c) -> Var m a (Event b) -> Var m a (Event c)
ringM cln = (go (const $ return ()) .) . tagM
    where go f ve = Var $ \a -> do (ec, ve') <- runVar ve a
                                   case ec of
                                       NoEvent -> return (ec, go f ve')
                                       Event c -> do f c
                                                     return (ec, go cln ve')

-- | Injects a monadic computation into the events of `vb`, providing a way
-- to perform side-effects inside an `Event` inside a `Var`.
tagM :: Monad m => (b -> m c) -> Var m a (Event b) -> Var m a (Event c)
tagM f vb = Var $ \a -> do
    (eb, vb') <- runVar vb a
    case eb of
        Event b -> do c <- f b
                      return (Event c, tagM f vb')
        NoEvent -> return (NoEvent, tagM f vb')
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
onTrue :: Monad m => Var m Bool (Event ())
onTrue = var $ \b -> if b then Event () else NoEvent

-- | Triggers an `Event a` when the input is `Just a`.
onJust :: Monad m => Var m (Maybe a) (Event a)
onJust = var $ \ma -> case ma of
                               Nothing -> NoEvent
                               Just a  -> Event a

-- | Triggers an `Event a` when the input is a unique value.
onUnique :: (Monad m, Eq a) => Var m a (Event a)
onUnique = trigger NoEvent
    where trigger NoEvent   = Var $ \a  -> return (Event a, trigger $ Event a)
          trigger (Event a) = Var $ \a' -> let e = if a == a'
                                                      then Event a
                                                      else Event a'
                                             in return (e, trigger e)

-- | Triggers an `Event a` when the condition is met.
onWhen :: Applicative m => (a -> Bool) -> Var m a (Event a)
onWhen f = var $ \a -> if f a then Event a else NoEvent

-- | Wraps all produced values of the given var with events.
toEvent :: Monad m => Var m a b -> Var m a (Event b)
toEvent = (~> var Event)
--------------------------------------------------------------------------------
-- Using event values
--------------------------------------------------------------------------------
-- | Collect all produced values into a monoidal structure using the given
-- insert function.
collectWith :: (Monoid b, Monad m) => (a -> b -> b) -> Var m (Event a) b
collectWith f = Var $ \a -> collect' mempty a
    where collect' b e = let b' = case e of
                                        NoEvent -> b
                                        Event a' -> f a' b
                          in return (b', Var $ \a' -> collect' b' a')

-- | Collect all produced values into a list.
collect :: Monad m => Var m (Event a) [a]
collect = collectWith (:)

-- | Produces the given value until the input events produce a value, then
-- produce that value until a new input event produces. This always holds
-- the last produced value, starting with the given value.
-- @
-- time ~> after 3 ~> startingWith 0
-- @
-- This is similar to `hold` except that it takes events from its input value
-- instead of another yarn.
startingWith, startWith :: Monad m => a -> Var m (Event a) a
startingWith = startWith
startWith a = Var $ \e ->
    return $ case e of
                 NoEvent  -> (a, startWith a)
                 Event a' -> (a', startWith a')

-- | Flipped version of `hold`.
holdWith :: Monad m => b -> Var m a (Event b) -> Var m a b
holdWith = flip hold

-- | Produces the `initial` value until the given yarn produces an event.
-- After an event is produced that event's value will be produced until the
-- next event produced by the given yarn.
hold :: Monad m => Var m a (Event b) -> b -> Var m a b
hold w initial = Var $ \x -> do
    (mb, w') <- runVar w x
    return $ case mb of
        NoEvent -> (initial, hold w' initial)
        Event e -> (e, hold w' e)

-- | Produce events after the first until the second. After a successful
-- cycle it will start over.
between :: Monad m => Var m a (Event b) -> Var m a (Event c) -> Var m a (Event ())
between vb vc = (never `before` vb) `andThenE` (toEvent vu `before` vc) `andThen` between vb vc
    where vu = pure ()

-- | Produce events with the initial value only after the input stream has
-- produced one event.
after :: Monad m => Var m a b -> Var m a (Event c) -> Var m a (Event b)
after vb ve = Var $ \a -> do
    (_, vb') <- runVar vb a
    (e, ve') <- runVar ve a
    case e of
        Event _ -> return (NoEvent, toEvent vb')
        NoEvent -> return (NoEvent, vb' `after` ve')

-- | Like before, but use the value produced by the switching stream to
-- create a stream to switch to.
beforeWith :: Monad m
           => Var m a b
           -> (Var m a (Event b), b -> Var m a (Event b))
           -> Var m a (Event b)
beforeWith vb (ve, f) = Var $ \a -> do
    (b, vb') <- runVar vb a
    (e, ve') <- runVar ve a
    case e of
        Event b' -> runVar (f b') a
        NoEvent  -> return (Event b, beforeWith vb' (ve', f))

-- | Like before, but sample the value of the second stream once before
-- inhibiting.
beforeOne :: Monad m => Var m a b -> Var m a (Event b) -> Var m a (Event b)
beforeOne vb ve = Var $ \a -> do
    (b, vb') <- runVar vb a
    (e, ve') <- runVar ve a
    case e of
        Event b' -> return (Event b', never)
        NoEvent  -> return (Event b, vb' `beforeOne` ve')

-- | Produce events with the initial varying value only before the second stream
-- has produced one event.
before :: Monad m => Var m a b -> Var m a (Event c) -> Var m a (Event b)
before = until

-- | Produce events with the initial varying value until the input event stream
-- `ve` produces its first event, then never produce any events.
until :: Monad m => Var m a b -> Var m a (Event c) -> Var m a (Event b)
until vb ve = Var $ \a -> do
    (b, vb') <- runVar vb a
    (e, ve') <- runVar ve a
    case e of
        Event _ -> return (NoEvent, never)
        NoEvent -> return (Event b, vb' `until` ve')

-- | Produce the given value once and then inhibit forever.
once :: Monad m => b -> Var m a (Event b)
once b = Var $ \_ -> return (Event b, never)

-- | Stream through some number of successful events and then inhibit forever.
takeE :: Monad m => Int -> Var m a (Event b) -> Var m a (Event b)
takeE n ve = Var $ \a -> do
    (eb, ve') <- runVar ve a
    case eb of
        NoEvent -> return (NoEvent, takeE n ve')
        Event b -> return (Event b, takeE (n-1) ve')

-- | Inhibit all events that don't pass the predicate.
filterE :: Monad m => (b -> Bool) -> Var m a (Event b) -> Var m a (Event b)
filterE p v = v ~> var check
    where check (Event b) = if p b then Event b else NoEvent
          check _ = NoEvent

-- | Never produces any event values.
never :: Monad m => Var m b (Event c)
never = pure NoEvent

-- | Produces events with the initial value forever.
--always :: Monad m => Var m a b -> Var m a (Event b)
--always = (~> var Event)
always :: Monad m => b -> Var m a (Event b)
always = pure . Event
--------------------------------------------------------------------------------
-- Switching yarn on events
--------------------------------------------------------------------------------
-- | Produces the first yarn's Event values until that stops producing, then
-- switches to the second yarn.
andThen :: Monad m => Var m a (Event b) -> Var m a b -> Var m a b
andThen w1 w2 = w1 `andThenWith` const w2

-- | Switches from one event stream to another once the first stops
-- producing.
andThenE :: Monad m => Var m a (Event b) -> Var m a (Event b) -> Var m a (Event b)
andThenE y1 y2 = Var $ \a -> do
    (e, y1') <- runVar y1 a
    case e of
        NoEvent -> runVar y2 a
        Event b -> return $ (Event b, y1' `andThenE` y2)

-- | Switches from one event stream when that stream stops producing. A new
-- stream is created using the last produced value (or `Nothing`) and used
-- as the second stream.
andThenWith :: Monad m
            => Var m a (Event b) -> (Maybe b -> Var m a b) -> Var m a b
andThenWith = go Nothing
    where go mb w1 f = Var $ \a -> do
              (e, w1') <- runVar w1 a
              case e of
                  NoEvent -> runVar (f mb) a
                             {-let w2 = f mb
                             in case mb of
                                 -- This is a bit of a hack to keep
                                 -- recursive vars from infinitely
                                 -- recursing. If we keep playing the same
                                 -- inhibiting signal with the same inputs
                                 -- it will continue recursing. In order to
                                 -- avoid that situation we are here using
                                 -- the last known value for this step, and
                                 -- will start producing with the new
                                 -- signal at the next step, with the next
                                 -- step's input value. There should be
                                 -- a way to do this using some other
                                 -- combinator 'onceAt' or 'at' ...
                                 -- instead of putting this
                                 -- here.
                                 Nothing -> runVar w2 a
                                 Just b  -> return (b, w2)-}
                  Event b -> return $ (b, go (Just b) w1' f)
--------------------------------------------------------------------------------
-- Operations on Events
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

instance Functor Event where
    fmap f (Event a) = Event $ f a
    fmap _ NoEvent = NoEvent

data Event a = Event a | NoEvent
