-- |
--   Module:     Control.Varying.SplineT
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  Using splines we can easily create continuous value streams from
--  multiple piecewise event streams. A spline is a monadic layer on top of
--  event streams which are only continuous over a certain domain. The idea
--  is that we use do notation to "run an event stream" from which we will
--  consume produced values. Once the event stream inhibits the computation
--  completes and returns a result value. That result value is then
--  used to determine the next spline in the sequence.
--
--  A spline can be converted back into a value stream using 'execSpline' or
--  'execSplineT'. This allows us to build long, complex, sequential behaviors
--  using familiar notation.
--
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Control.Varying.Spline (
    -- * Spline
    Spline,
    -- * Spline Transformer
    SplineT(..),
    runSplineT,
    scanSpline,
    outputStream,
    resultStream,
    -- * Combinators
    step,
    untilEvent,
    untilEvent_,
    _untilEvent,
    _untilEvent_,
    race,
    capture,
    mapOutput,
    adjustInput,
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Functor.Identity

data SplineT a b m c where
  Pass :: c -> SplineT a b m c
  SplineT :: VarT m a (b, Event c) -> SplineT a b m c

instance Monad m => Functor (SplineT a b m) where
  fmap f (Pass c) = Pass $ f c
  fmap f (SplineT v) = SplineT (((f <$>) <$>) <$> v)

runSplineT :: Monad m => SplineT a b m c -> b -> VarT m a (b, Event c)
runSplineT (Pass c) b = pure (b, Event c)
runSplineT (SplineT v) _ = VarT $ \a -> do
  (o@(b,ec), v1) <- runVarT v a
  let s = case ec of
              NoEvent -> SplineT v1
              Event c -> Pass c
  return (o, runSplineT s b)

instance Monad m => Applicative (SplineT a b m) where
  pure = Pass
  (Pass f) <*> (Pass x) = Pass $ f x
  (Pass f) <*> (SplineT v) = f <$> SplineT v
  (SplineT vf) <*> (Pass x) = ($ x) <$> SplineT vf
  (SplineT vf) <*> (SplineT vx) = SplineT $ VarT $ \a -> do
    ((_,ef), vf1) <- runVarT vf a
    ((b,ex), vx1) <- runVarT vx a
    let s = SplineT vf1 <*> SplineT vx1
    return ((b, ef <*> ex), runSplineT s b)

instance Monad m => Monad (SplineT a b m) where
  return = pure
  (Pass x) >>= f = f x
  (SplineT v) >>= f = SplineT $ VarT $ \a -> do
    ((b, ec), v1) <- runVarT v a
    let boundf = flip runSplineT b $ SplineT v1 >>= f
        payload = (b, NoEvent)
    case ec of
      NoEvent -> return (payload, boundf)
      Event c -> runVarT (runSplineT (f c) b) a

-- | Run the spline over the input values, gathering the output and result
-- values in a list.
scanSpline :: (Applicative m, Monad m)
           => SplineT a b m c -> b -> [a] -> m [b]
scanSpline s b = scanVar (outputStream s b)

-- | A SplineT monad parameterized with Identity that takes input of type @a@,
-- output of type @b@ and a result value of type @c@.
type Spline a b c = SplineT a b Identity c

-- | Evaluates a spline into a value stream of its output type.
outputStream :: (Applicative m, Monad m)
             => SplineT a b m c -> b -> VarT m a b
outputStream s b = fst <$> runSplineT s b

resultStream :: (Applicative m, Monad m)
             => SplineT a b m c -> b -> VarT m a (Event c)
resultStream s b = snd <$> runSplineT s b
-- | Create a spline from a value stream and an event stream. The spline
-- uses the value stream as its output value. The spline will run until
-- the event stream produces a value, at that point the last output
-- value and the event value are tupled and returned as the spline's result
-- value.
untilEvent :: (Applicative m, Monad m)
           => VarT m a b -> VarT m a (Event c)
           -> SplineT a b m (b,c)
untilEvent v ve = SplineT $ f <$> v <*> ve
  where f b ec = (b, (b,) <$> ec)

-- | A variant of 'untilEvent' that only results in the left result,
-- discarding the right result.
untilEvent_ :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c)
            -> SplineT a b m b
untilEvent_ v ve = SplineT $ f <$> v <*> ve
  where f b ec = (b, b <$ ec)

-- | A variant of 'untilEvent' that only results in the right result,
-- discarding the left result.
_untilEvent :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c)
            -> SplineT a b m c
_untilEvent v ve = snd <$> untilEvent v ve

-- | A variant of 'untilEvent' that discards both the right and left results.
_untilEvent_ :: (Applicative m, Monad m)
             => VarT m a b -> VarT m a (Event c)
             -> SplineT a b m ()
_untilEvent_ v ve = void $ _untilEvent v ve

-- | Run two splines in parallel, combining their output. Return the result of
-- the spline that concludes first. If they conclude at the same time the result
-- is taken from the left spline.
race :: (Applicative m, Monad m)
     => (a -> b -> c) -> SplineT i a m d -> SplineT i b m e -> SplineT i c m (Either d e)
race _ (Pass x) _ = Pass $ Left x
race _ _ (Pass x) = Pass $ Right x
race f (SplineT va) (SplineT vb) = SplineT $ VarT $ \i -> do
    ((a, ed), va1) <- runVarT va i
    ((b, ee), vb1) <- runVarT vb i
    let c = f a b
    case (ed,ee) of
        (Event d,_) -> return ( (c, Event $ Left d), pure (c, Event $ Left d))
        (_,Event e) -> return ( (c, Event $ Right e), pure (c, Event $ Right e))
        (_,_)       -> return ( (c, NoEvent)
                         , runSplineT (race f (SplineT va1) (SplineT vb1)) c
                         )

-- | Capture the spline's last output value and tuple it with the
-- spline's result. This is helpful when you want to sample the last
-- output value in order to determine the next spline to sequence.
capture :: (Applicative m, Monad m, Eq b)
        => SplineT a b m c -> SplineT a b m (Maybe b, c)
capture (Pass x) = Pass (Nothing, x)
capture (SplineT v) = capture' v
    where capture' v' = SplineT $ VarT $ \a -> do
              ((b, ec), v'') <- runVarT v' a
              let mb' = Just b
              return ((b, (mb',) <$> ec), runSplineT (capture' v'') b)

-- | Produce the argument as an output value exactly once.
step :: (Applicative m, Monad m) => b -> SplineT a b m ()
step b = SplineT $ VarT $ \_ -> return ((b, NoEvent), pure (b,Event ()))

-- | Map the output value of a spline.
mapOutput :: (Applicative m, Monad m)
          => VarT m a (b -> t) -> SplineT a b m c -> SplineT a t m c
mapOutput vf (SplineT vx) = SplineT $ vg <*> vx
    where vg = (\f (b,ec) -> (f b, ec)) <$> vf
mapOutput _ (Pass c) = Pass c

-- | Map the input value of a spline.
adjustInput :: (Monad m)
            => VarT m a (a -> r) -> SplineT r b m c -> SplineT a b m c
adjustInput vf (SplineT vx) = SplineT $ VarT $ \a -> do
    (f, vf1) <- runVarT vf a
    (b, vx1) <- runVarT vx $ f a
    return (b, runSplineT (adjustInput vf1 $ SplineT vx1) $ fst b)
adjustInput _ (Pass c) = Pass c
