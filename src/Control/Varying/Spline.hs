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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Control.Varying.Spline (
    -- * Spline
    Spline,
    -- * Spline Transformer
    SplineT(..),
    -- * Running and streaming
    scanSpline,
    outputStream,
    resultStream,
    -- * Combinators
    step,
    effect,
    fromEvent,
    untilEvent,
    untilEvent_,
    _untilEvent,
    _untilEvent_,
    race,
    raceMany,
    merge,
    capture,
    mapOutput,
    adjustInput,
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Data.Functor.Identity
import Data.Monoid

-- | 'SplineT' shares all the types of 'VarT' and adds a result value. Its
-- monad, input and output types (@m@, @a@ and @b@, respectively) reflect the
-- underlying 'VarT`. A spline adds a result type which represents the monadic
-- computation's result value.
-- Much like the State monad it has an "internal state" and an eventual
-- result value, where the internal state is the output value. The result
-- value is used only in determining the next spline to sequence.
newtype SplineT a b m c = SplineT { unSplineT :: VarT m a (Either b c) }

-- | A spline responds to bind by running until it produces an eventual value,
-- then uses that value to run the next spline.
instance (Applicative m, Monad m) => Monad (SplineT a b m) where
  return x = SplineT $ VarT $ const $ return (Right x, done $ Right x)
  s >>= f = SplineT $ VarT $ g $ unSplineT s
    where g v (!a) = do (e, v1) <- runVarT v a
                        case e of
                          Left  (!b) -> return (Left b, VarT $ g v1)
                          Right (!c) -> runVarT (unSplineT $ f c) a

-- | A spline is a functor by applying the function to the result.
instance (Applicative m, Monad m) => Functor (SplineT a b m) where
  fmap f (SplineT v) = SplineT ((f <$>) <$> v)
--
---- A spline responds to 'pure' by returning a spline that never produces an
---- output value and immediately returns the argument. It responds to '<*>' by
---- applying the left arguments result value (the function) to the right
---- arguments result value (the argument), sequencing them both in serial.
instance (Applicative m, Monad m) => Applicative (SplineT a b m) where
  pure = return
  sf <*> sx = do
    f <- sf
    x <- sx
    return $ f x

-- | Run the side effect and use its result as the spline's result. This
-- discards the output argument and switches immediately, but the argument is
-- needed to construct the spline. For this reason spline's can't be an instance
-- of MonadTrans or MonadIO.
effect :: (Applicative m, Monad m) => m x -> SplineT a b m x
effect f = SplineT $ VarT $ const $ do
  x <- f
  return (Right x, done $ Right x)

#if MIN_VERSION_base(4,8,0)
-- | A spline is a transformer by using @effect@.
instance MonadTrans (SplineT a b) where
  lift = effect

-- | A spline can do IO if its underlying monad has a MonadIO instance. It
-- takes the result of the IO action as its immediate return value.
instance (Monoid b, Applicative m, Monad m, MonadIO m) => MonadIO (SplineT a b m) where
  liftIO = lift . liftIO
#endif

-- | A SplineT monad parameterized with Identity that takes input of type @a@,
-- output of type @b@ and a result value of type @c@.
type Spline a b c = SplineT a b Identity c

-- | Evaluates a spline into a value stream of its output type.
outputStream :: (Applicative m, Monad m)
             => SplineT a b m c -> b -> VarT m a b
outputStream s b0 = VarT $ f (unSplineT s) b0
  where f v0 (!b) (!a) = do
          (e, v) <- runVarT v0 a
          return $ case e of
            Left b1 -> (b1, VarT $ f v b1)
            Right _ -> (b , done b)

-- | Run the spline over the input values, gathering the output values in a
-- list.
scanSpline :: (Applicative m, Monad m)
           => SplineT a b m c -> b -> [a] -> m [b]
scanSpline s b = fmap fst <$> scanVar (outputStream s b)

resultStream :: (Applicative m, Monad m)
             => SplineT a b m c -> VarT m a (Maybe c)
resultStream s = VarT $ f (unSplineT s)
  where f v0 (!a) = do
          (e, v) <- runVarT v0 a
          return $ case e of
            Left _  -> (Nothing, VarT $ f v)
            Right c -> (Just c, done $ Just c)

-- | Create a spline from an event stream.
fromEvent :: (Applicative m, Monad m) => VarT m a (Event b) -> SplineT a (Event b) m b
fromEvent ve = SplineT $ f <$> ve
  where f (Event b) = Right b
        f e = Left e

-- | Create a spline from a value stream and an event stream. The spline
-- uses the value stream as its output value. The spline will run until
-- the event stream produces a value, at that point the last output
-- value and the event value are tupled and returned as the spline's result
-- value.
untilEvent :: (Applicative m, Monad m)
           => VarT m a b -> VarT m a (Event c)
           -> SplineT a b m (b,c)
untilEvent v ve = SplineT $ f <$> v <*> ve
  where f b (Event c) = Right (b, c)
        f b _         = Left b

-- | A variant of 'untilEvent' that only results in the left result,
-- discarding the right result.
untilEvent_ :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c)
            -> SplineT a b m b
untilEvent_ v ve = SplineT $ f <$> v <*> ve
  where f b (Event _) = Right b
        f b _ = Left b

-- | A variant of 'untilEvent' that only results in the right result,
-- discarding the left result.
_untilEvent :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c)
            -> SplineT a b m c
_untilEvent v ve = snd <$> untilEvent v ve

---- | A variant of 'untilEvent' that discards both the right and left results.
_untilEvent_ :: (Applicative m, Monad m)
             => VarT m a b -> VarT m a (Event c)
             -> SplineT a b m ()
_untilEvent_ v ve = void $ _untilEvent v ve

-- | Run two splines in parallel, combining their output. Return the result of
-- the spline that concludes first. If they conclude at the same time the result
-- is taken from the left spline.
race :: (Applicative m, Monad m)
     => (a -> b -> c) -> SplineT i a m d -> SplineT i b m e
     -> SplineT i c m (Either d e)
race f (SplineT va) (SplineT vb) = SplineT $ VarT $ go va vb
  where go va1 vb1 (!i) = runVarT va1 i >>= \case
          (Right d,   _) -> return (Right $ Left d, done $ Right $ Left d)
          (Left  a, va2) -> runVarT vb1 i >>= \case
            (Right e,   _) -> return (Right $ Right e, done $ Right $ Right e)
            (Left  b, vb2) -> return (Left $ f a b, VarT $ go va2 vb2)

raceMany :: (Applicative m, Monad m, Monoid b)
         => [SplineT a b m c] -> SplineT a b m c
raceMany [] = pure mempty `_untilEvent` never
raceMany ss = SplineT $ VarT $ f [] (map unSplineT ss) mempty
  where f ys []     b _    = return (Left b, VarT $ f [] ys mempty)
        f ys (v:vs) b (!a) = runVarT v a >>= \case
          (Right c, _)   -> return (Right c, done $ Right c)
          (Left  b1, v1) -> f (ys ++ [v1]) vs (b <> b1) a

-- | Run two splines in parallel, combining their output. Once both splines
-- have concluded, return the results of each in a tuple.
merge :: (Applicative m, Monad m)
     => (b -> b -> b)
     -> SplineT a b m c -> SplineT a b m d -> SplineT a b m (c, d)
merge apnd s1 s2 = SplineT $ VarT $ f (unSplineT s1) (unSplineT s2)

  where r c d = let e = (c, d) in return (Right e, done $ Right e)

        fr c vb (!a) = runVarT vb a >>= \case
          (Right d,   _) -> r c d
          ( Left b, vb1) -> return (Left b, VarT $ fr c vb1)

        fl d va (!a) = runVarT va a >>= \case
          (Right c,   _) -> r c d
          ( Left b, va1) -> return (Left b, VarT $ fl d va1)

        f va vb (!a) = runVarT va a >>= \case
          (Right c,   _) -> fr c vb a
          (Left b1, va1) -> runVarT vb a >>= \case
            (Right d, _)   -> return (Left b1, VarT $ fl d va1)
            (Left b2, vb1) -> return (Left $ apnd b1 b2, VarT $ f va1 vb1)

-- | Capture the spline's last output value and tuple it with the
-- spline's result. This is helpful when you want to sample the last
-- output value in order to determine the next spline to sequence.
capture :: (Applicative m, Monad m)
        => SplineT a b m c -> SplineT a b m (Maybe b, c)
capture s = SplineT $ VarT $ f Nothing $ unSplineT s
    where f mb v (!a) = runVarT v a >>= \case
            (Right c, _)  -> return (Right (mb, c), done $ Right (mb, c))
            (Left  b, v1) -> return (Left b, VarT $ f (Just b) v1)

-- | Produce the argument as an output value exactly once.
step :: (Applicative m, Monad m) => b -> SplineT a b m ()
step b = SplineT $ VarT $ const $ return (Left b, done $ Right ())

-- | Map the output value of a spline.
mapOutput :: (Applicative m, Monad m)
          => VarT m a (b -> t) -> SplineT a b m c -> SplineT a t m c
mapOutput vf (SplineT vx) = SplineT $ (g <$> vf) <*> vx
    where g f (Left  b) = Left $ f b
          g _ (Right c) = Right c

-- | Map the input value of a spline.
adjustInput :: (Applicative m, Monad m)
            => VarT m a (a -> r) -> SplineT r b m c -> SplineT a b m c
adjustInput vf0 s = SplineT $ VarT $ g vf0 $ unSplineT s
  where g vf vx (!a) = do
          (f, vf1) <- runVarT vf a
          (b, vx1) <- runVarT vx $ f a
          return (b, VarT $ g vf1 vx1)
