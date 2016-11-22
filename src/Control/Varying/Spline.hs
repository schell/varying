-- |
--   Module:     Control.Varying.SplineT
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  Using splines we can easily create continuous streams from discontinuous
--  streams. A spline is a monadic layer on top of event streams which are only
--  continuous over a certain domain. The idea is that we use a monad to
--  "run a stream switched by events". This means taking two streams - an output
--  stream and an event stream, and combining them into a temporarily producing
--  stream. Once that "stream pair" inhibits, the computation completes and
--  returns a result value. That result value is then used to determine the next
--  spline in the sequence.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Control.Varying.Spline
  ( -- * Spline
    Spline
    -- * Spline Transformer
  , SplineT(..)
    -- * Creating streams from splines
  , outputStream
    -- * Creating splines from streams
  , untilEvent
  , untilEvent_
  , _untilEvent
  , _untilEvent_
    -- * Other runners
  , scanSpline
    -- * Combinators
  , step
  , race
  , raceAny
  , merge
  , capture
  , mapOutput
  , adjustInput
    -- * Helpers for debugging
  , fromEvent
  ) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Data.Functor.Identity
import Data.Function
import Data.Monoid

-- | 'SplineT' shares all the types of 'VarT' and adds a result value. Its
-- monad, input and output types (@m@, @a@ and @b@, respectively) represent the
-- same parameters in 'VarT`. A spline adds a result type which represents the
-- monadic computation's result value.
-- A spline either concludes in a result or it produces an output value and
-- another spline. This makes it a stream that eventually ends. We can use this
-- to set up our streams in a monadic fasion, where the end result of one spline
-- can be used to determin the next spline to run. Using 'outputStream' we can
-- then fuse these piecewise continuous (but otherwise discontinuous) streams
-- into one continuous stream of type 'VarT m a b'.
newtype SplineT a b m c = SplineT { runSplineT :: a -> m (Either c (b, SplineT a b m c)) }

-- | A spline is a functor by applying the function to the result of the
-- spline.
instance (Applicative m, Monad m) => Functor (SplineT a b m) where
  fmap f (SplineT s) = SplineT $ \a -> s a >>= \case
    Left c        -> return $ Left $ f c
    Right (b, s1) -> return $ Right (b, fmap f s1)

-- | A spline responds to bind by running until it concludes in a value,
-- then uses that value to run the next spline.
instance (Applicative m, Monad m) => Monad (SplineT a b m) where
  return = SplineT . const . return . Left
  (SplineT s0) >>= f = SplineT $ g s0
    where g s a = do e <- s a
                     case e of
                       Left  c               -> runSplineT (f c) a
                       Right (b, SplineT s1) -> return $ Right (b, SplineT $ g s1)

-- A spline responds to 'pure' by returning a spline that never produces an
-- output value and immediately returns the argument. It responds to '<*>' by
-- applying the left arguments result value (the function) to the right
-- arguments result value (the argument), sequencing them both in serial.
instance (Applicative m, Monad m) => Applicative (SplineT a b m) where
  pure = return
  sf <*> sx = do
    f <- sf
    x <- sx
    return $ f x

-- | A spline is a transformer by using @effect@.
instance MonadTrans (SplineT a b) where
  lift f = SplineT $ const $ f >>= return . Left

-- | A spline can do IO if its underlying monad has a MonadIO instance. It
-- takes the result of the IO action as its immediate return value.
instance (Applicative m, Monad m, MonadIO m) => MonadIO (SplineT a b m) where
  liftIO = lift . liftIO
-- #endif
--
-- | A SplineT monad parameterized with Identity that takes input of type @a@,
-- output of type @b@ and a result value of type @c@.
type Spline a b c = SplineT a b Identity c

-- | Permute a spline into one continuous stream. Since a spline is not
-- guaranteed to be defined over any domain (specifically on its edges), this
-- function takes a default value to use as the "last known value".
outputStream :: (Applicative m, Monad m)
             => SplineT a b m c -> b -> VarT m a b
outputStream (SplineT s0) b0 = VarT $ f s0 b0
  where f s b a = do e <- s a
                     case e of
                       Left  _                -> return (b, done b)
                       Right (b1, SplineT s1) -> return (b1, VarT $ f s1 b1)

-- | Run the spline over the input values, gathering the output values in a
-- list.
scanSpline :: (Applicative m, Monad m)
           => SplineT a b m c -> b -> [a] -> m [b]
scanSpline s b = fmap fst <$> scanVar (outputStream s b)

-- | Create a spline from an event stream.
fromEvent :: (Applicative m, Monad m) => VarT m a (Event b) -> SplineT a (Event b) m b
fromEvent ve = SplineT $ \a -> do
  (e, ve1) <- runVarT ve a
  return $ case e of
    Just b -> Left b
    Nothing -> Right (Nothing, fromEvent ve1)

-- | Create a spline from a stream and an event stream. The spline
-- uses the stream as its output value. The spline will run until
-- the event stream produces an event, at that point the last known output
-- value and the event value are tupled and returned as the spline's result.
untilEvent :: (Applicative m, Monad m)
           => VarT m a b -> VarT m a (Event c) -> SplineT a b m (b,c)
untilEvent v ve = SplineT $ f ((,) <$> v <*> ve)
  where f vve a = do t <-runVarT vve a
                     return $ case t of
                       ((b, Nothing), vve1) -> Right (b, SplineT $ f vve1)
                       ((b, Just c),    _) -> Left (b, c)

-- | A variant of 'untilEvent' that results in the last known output value.
untilEvent_ :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c) -> SplineT a b m b
untilEvent_ v ve = fst <$> untilEvent v ve

-- | A variant of 'untilEvent' that results in the event steam's event value.
_untilEvent :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c) -> SplineT a b m c
_untilEvent v ve = snd <$> untilEvent v ve

-- | A variant of 'untilEvent' that discards both the output and event values.
_untilEvent_ :: (Applicative m, Monad m)
             => VarT m a b -> VarT m a (Event c) -> SplineT a b m ()
_untilEvent_ v ve = void $ _untilEvent v ve

-- | Run two splines in parallel, combining their output. Return the result of
-- the spline that concludes first. If they conclude at the same time the result
-- is taken from the left spline.
race :: (Applicative m, Monad m)
     => (a -> b -> c) -> SplineT i a m d -> SplineT i b m e
     -> SplineT i c m (Either d e)
race f sa0 sb0 = SplineT (g sa0 sb0)
  where g sa sb i = runSplineT sa i >>= \case
          Left d -> return $ Left $ Left d
          Right (a, sa1) -> runSplineT sb i >>= \case
            Left e -> return $ Left $ Right e
            Right (b, sb1) -> return $ Right (f a b, SplineT $ g sa1 sb1)

-- | Run many splines in parallel, combining their output with 'mappend'.
-- Returns the result of the spline that concludes first. If any conclude at the
-- same time the leftmost result will be returned.
raceAny :: (Applicative m, Monad m, Monoid b)
         => [SplineT a b m c] -> SplineT a b m c
raceAny [] = pure mempty `_untilEvent` never
raceAny ss = SplineT $ f [] (map runSplineT ss) mempty
  where f ys []     b _    = return $ Right (b, SplineT $ f [] ys mempty)
        f ys (v:vs) b a = v a >>= \case
          Left c -> return $ Left c
          Right (b1, s) -> f (ys ++ [runSplineT s]) vs (b <> b1) a

-- | Run two splines in parallel, combining their output. Once both splines
-- have concluded, return the results of each in a tuple.
merge :: (Applicative m, Monad m)
     => (b -> b -> b)
     -> SplineT a b m c -> SplineT a b m d -> SplineT a b m (c, d)
merge apnd s1 s2 = SplineT $ f s1 s2

  where r c d = return $ Left (c, d)

        fr c vb a = runSplineT vb a >>= \case
          Left d -> r c d
          Right (b, vb1) -> return $ Right (b, SplineT $ fr c vb1)

        fl d va a = runSplineT va a >>= \case
          Left c -> r c d
          Right (b, va1) -> return $ Right (b, SplineT $ fl d va1)

        f va vb a = runSplineT va a >>= \case
          Left c -> fr c vb a
          Right (b1, va1) -> runSplineT vb a >>= \case
            Left d -> return $ Right (b1, SplineT $ fl d va1)
            Right (b2, vb1) -> return $ Right $ (apnd b1 b2, SplineT $ f va1 vb1)

-- | Capture the spline's last output value and tuple it with the
-- spline's result. This is helpful when you want to sample the last
-- output value in order to determine the next spline to sequence.
capture :: (Applicative m, Monad m)
        => SplineT a b m c -> SplineT a b m (Event b, c)
capture = SplineT . f Nothing
    where f mb s a = runSplineT s a >>= \case
            Left c -> return $ Left (mb, c)
            Right (b, s1) -> return $ Right (b, SplineT $ f (Just b) s1)

-- | Produce the argument as an output value exactly once.
step :: (Applicative m, Monad m) => b -> SplineT a b m ()
step b = SplineT $ const $ return $ Right (b, return ())

-- | Map the output value of a spline.
mapOutput :: (Applicative m, Monad m)
          => VarT m a (b -> t) -> SplineT a b m c -> SplineT a t m c
mapOutput vf0 s0 = SplineT $ g vf0 s0
    where g vf s a = do
            (f, vf1) <- runVarT vf a
            runSplineT s a >>= \case
              Left c -> return $ Left c
              Right (b, s1) -> return $ Right (f b, SplineT $ g vf1 s1)

-- | Map the input value of a spline.
adjustInput :: (Applicative m, Monad m)
            => VarT m a (a -> r) -> SplineT r b m c -> SplineT a b m c
adjustInput vf0 s = SplineT $ g vf0 s
  where g vf sx (!a) = do
          (f, vf1) <- runVarT vf a
          runSplineT sx (f a) >>= \case
           Left c -> return $ Left c
           Right (b, sx1) -> return $ Right (b, SplineT $ g vf1 sx1)
