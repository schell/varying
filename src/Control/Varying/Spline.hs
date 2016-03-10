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
    effect,
    untilEvent,
    untilEvent_,
    _untilEvent,
    _untilEvent_,
    race,
    merge,
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

-- | 'SplineT' shares all the types of 'VarT' and adds a result value. Its
-- monad, input and output types (@m@, @a@ and @b@, respectively) reflect the
-- underlying 'VarT`. A spline adds a result type which represents the monadic
-- computation's result value.
-- Much like the State monad it has an "internal state" and an eventual
-- result value, where the internal state is the output value. The result
-- value is used only in determining the next spline to sequence.
data SplineT a b m c where
  Pass :: c -> SplineT a b m c
  SplineT :: VarT m a (b, Event c) -> SplineT a b m c

-- | Convert a spline into a stream of output value and eventual result value
-- tuples. Requires a default output value in case none are produced.
runSplineT :: Monad m => SplineT a b m c -> b -> VarT m a (b, Event c)
runSplineT (Pass c) b = pure (b, Event c)
runSplineT (SplineT v) _ = VarT $ \a -> do
  (o@(b,ec), v1) <- runVarT v a
  let s = case ec of
              NoEvent -> SplineT v1
              Event c -> Pass c
  return (o, runSplineT s b)

-- | A spline is a functor by applying the function to the result.
instance Monad m => Functor (SplineT a b m) where
  fmap f (Pass c) = Pass $ f c
  fmap f (SplineT v) = SplineT (((f <$>) <$>) <$> v)

-- A spline responds to 'pure' by returning a spline that never produces an
-- output value and immediately returns the argument. It responds to '<*>' by
-- applying the left arguments result value (the function) to the right
-- arguments result value (the argument), sequencing them both in serial.
instance Monad m => Applicative (SplineT a b m) where
  pure = Pass
  (Pass f) <*> (Pass x) = Pass $ f x
  (Pass f) <*> (SplineT v) = f <$> SplineT v
  (SplineT vf) <*> (Pass x) = ($ x) <$> SplineT vf
  sf <*> sx = do
    f <- sf
    x <- sx
    return $ f x

-- | A spline responds to bind by running until it produces an eventual value,
-- then uses that value to run the next spline.
instance Monad m => Monad (SplineT a b m) where
  return = Pass
  (Pass x) >>= f = f x
  (SplineT v) >>= f = SplineT $ VarT $ \a -> do
    ((b, ec), v1) <- runVarT v a
    case ec of
      NoEvent -> return ((b, NoEvent), runSplineT (SplineT v1 >>= f) b)
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

-- | Run two splines in parallel, combining their output. Once both splines
-- have concluded, return the results of each in a tuple.
merge :: (Applicative m, Monad m)
     => (b -> b -> b) -> (c -> d -> e)
     -> SplineT a b m c -> SplineT a b m d -> SplineT a b m e
merge _ g (Pass c) (Pass d) = Pass $ g c d
merge _ g (Pass c) s = g c <$> s
merge _ g s (Pass d) = flip g d <$> s
merge f g (SplineT v1) (SplineT v2) = SplineT $ VarT $ \a -> do
  ((b1,e1), v3) <- runVarT v1 a
  ((b2,e2), v4) <- runVarT v2 a
  let b = f b1 b2
  case (e1,e2) of
    (Event c, Event d) -> let e = (b, Event $ g c d) in return (e, pure e)
    (Event _, _) -> do let s = SplineT $ pure (b1,e1)
                           sv4 = SplineT v4
                       return ((b, NoEvent), runSplineT (merge f g s sv4) b)
    (_, Event _) -> do let s = SplineT $ pure (b2,e2)
                           sv3 = SplineT v3
                       return ((b, NoEvent), runSplineT (merge f g sv3 s) b)
    _ -> do let sv3 = SplineT v3
                sv4 = SplineT v4
            return ((b, NoEvent), runSplineT (merge f g sv3 sv4) b)

-- | Run the side effect and use its result as the spline's result. This
-- discards the output argument and switches immediately, but the argument is
-- needed to construct the spline. For this reason spline's can't be an instance
-- of MonadTrans or MonadIO.
effect :: (Applicative m, Monad m) => b -> m x -> SplineT a b m x
effect b f = SplineT $ VarT $ const $ do
  x <- f
  return ((b, Event x), pure (b, Event x))

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
