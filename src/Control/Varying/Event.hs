{-# LANGUAGE Arrows #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Control.Varying.Event (
    Event(..),
    -- * Transforming event values.
    toMaybe,
    -- * Combining events and values
    latchWith,
    orE,
    tagOn,
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
    always,
    never,
    -- * Switching and chaining events
    andThen,
    andThenWith,
    andThenE,
    untilE,
    untilWithE
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
latchWith :: Monad m
          => (b -> c -> d) -> Var m a (Event b) -> Var m a (Event c) -> Var m a (Event d)
latchWith f vb vc = latchWith' (NoEvent, vb) vc
    where latchWith' (eb, vb') vc' =
              Var $ \e -> do (eb', vb'') <- runVar vb' e
                             (ec', vc'') <- runVar vc' e
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

tagOn :: Monad m => Var m a b -> Var m a (Event c) -> Var m a (Event b)
tagOn vb ve = proc a -> do
    b <- vb -< a
    e <- ve -< a
    returnA -< b <$ e
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
startWith a = Var $ \ma ->
    return $ case ma of
                 NoEvent  -> (a, (startWith) a)
                 Event a' -> (a', (startWith) a')

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
between vb vc = (never `before` vb) `andThenE` (always vu `before` vc) `andThen` between vb vc
    where vu = pure ()

-- | Produce events with the initial value only after the input stream has
-- produced one event.
after :: Monad m => Var m a b -> Var m a (Event c) -> Var m a (Event b)
after vb ve = Var $ \a -> do
    (_, vb') <- runVar vb a
    (e, ve') <- runVar ve a
    case e of
        Event _ -> return (NoEvent, always vb')
        NoEvent -> return (NoEvent, vb' `after` ve')

-- | Produce events with the initial varying value only before the second stream
-- has produced one event.
before :: Monad m => Var m a b -> Var m a (Event c) -> Var m a (Event b)
before = until

-- | Produce events with the initial varying value until the input stream
-- produces its first event, then never produce any events.
until :: Monad m => Var m a b -> Var m a (Event d) -> Var m a (Event b)
until vb ve = Var $ \a -> do
    (b, vb') <- runVar vb a
    (e, ve') <- runVar ve a
    case e of
        Event _ -> return (NoEvent, never)
        NoEvent -> return (Event b, vb' `until` ve')

-- | Produce

-- | Never produces any event values.
never :: Monad m => Var m b (Event c)
never = pure NoEvent

-- | Produces events with the initial value forever.
always :: Monad m => Var m a b -> Var m a (Event b)
always = (~> var Event)
--------------------------------------------------------------------------------
-- Switching yarn on events
--------------------------------------------------------------------------------
-- | Produces the first yarn's Event values until that stops producing, then
-- switches to the second yarn.
andThen :: Monad m => Var m a (Event b) -> Var m a b -> Var m a b
andThen w1 w2 = w1 `andThenWith` const w2

-- | Produces the first yarn's Event values until that stops producing,
-- then switches to the second yarn's Event values.
andThenE :: Monad m => Var m a (Event b) -> Var m a (Event b) -> Var m a (Event b)
andThenE y1 y2 = Var $ \a -> do
    (e, y1') <- runVar y1 a
    case e of
        NoEvent -> runVar y2 a
        Event b -> return $ (Event b, y1' `andThenE` y2)

-- | Produces a Event values of the first yarn's values until the second
-- yarn produces an Event value, then produces Event values of the second.
untilE :: Monad m => Var m a b -> Var m a (Event b) -> Var m a (Event b)
untilE w1 w2 = Var $ \a -> do
    (mb, w2') <- runVar w2 a
    case mb of
        NoEvent -> do (b, w1') <- runVar w1 a
                      return $ (Event b, w1' `untilE` w2')
        _  -> return $ (mb, w2')

untilWithE :: Monad m => Var m a b -> Var m a (Event c) -> (c -> Var m a b)
           -> Var m a b
untilWithE y ey f = Var $ \a -> do
    (mb, ey') <- runVar ey a
    case mb of
        NoEvent -> do (b, y') <- runVar y a
                      return $ (b, ((y') `untilWithE`) ey' f)
        Event b -> runVar (f b) a

andThenWith :: Monad m => Var m a (Event b) -> (Maybe b -> Var m a b) -> Var m a b
andThenWith = go Nothing
    where go mb w1 f = Var $ \a -> do
              (e, w1') <- runVar w1 a
              case e of
                  NoEvent -> runVar (f mb) a
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
