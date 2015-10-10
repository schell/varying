-- |
--   Module:     Control.Varying.SplineT
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  Using 'SplineT' we can easily create continuous varying values from
--  multiple piecewise varying values. A spline is a monadic layer on top of
--  automaton based varying values that are only piecewise continuous,
--  allowing us to build up varying values that are continuous over greater
--  and greater domains.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Varying.Spline (
    Step(..),
    SplineT(..),
    Spline,
    runSplineT,
    evalSplineT,
    execSplineT,
    spline,
    varyUntilEvent
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad.IO.Class
import Control.Applicative
import Data.Monoid

-- | A discrete step in a continuous function. This is simply a type that
-- discretely describes an eventual value on the right and a monoidal iteration
-- value on the left.
data Step f b where
    Step :: Monoid f => f -> Event b -> Step f b

-- | Returns the left value of a step.
stepIter :: Step f b -> f
stepIter (Step a _) = a

-- | Returns the right value of a step.
stepResult :: Step f b -> Event b
stepResult (Step _ b) = b

-- | A discrete step is a functor by applying a function to the contained
-- event's value.
instance Functor (Step f) where
    fmap f (Step a b) = Step a $ fmap f b

-- | A discrete spline is a monoid if its left and right types are monoids.
instance (Monoid f, Monoid b) => Monoid (Step f b) where
    mempty = Step mempty (Event mempty)
    mappend (Step a ea) (Step b eb) = Step (mappend a b) (mappend <$> ea <*> eb)

-- | A discrete spline is an applicative if its left datatype is a monoid. It
-- replies to 'pure' with an empty left value while the right value is the
-- argument wrapped in an event. It means "the argument happens instantly".
instance Monoid f => Applicative (Step f) where
    pure a = Step mempty $ Event a
    (Step uia f) <*> (Step uib b) = Step (mappend uia uib) (f <*> b)

-- | A spline is a varying value that is continuous over a domain. A spline
-- can end in a result value of a different type than its iteration type.
-- Much like the State monad it has an "internal state" and an eventual
-- return type. In most cases the internal state (the iteration value) is
-- the interesting part of the spline, but sometimes it's useful to use the
-- return value in determining the next spline.
data SplineT m f a b c = SplineT { unSplineT :: Var m a (Step (f b) c) }
                       | SplineTConst c

runSplineT :: (Monad m, Monoid (f b))
           => SplineT m f a b c -> Var m a (Step (f b) c)
runSplineT (SplineT v) = v
runSplineT (SplineTConst x) = pure $ pure x

-- | For most use cases (like Int, Float, Double - essentially any numbers)
-- a spline will use Event as its iterator container. This means that new
-- values overwrite/replace old values due to Event's 'Last'-like monoid
-- instance.
type Spline m a b c = SplineT m Event a b c

-- | A spline is a functor by applying the function to the result.
instance (Applicative m, Monad m) => Functor (SplineT m f a b) where
    fmap f (SplineT v) = SplineT $ fmap (fmap f) v
    fmap f (SplineTConst c)  = SplineTConst $ f c

-- | A spline is an applicative if its iteration type is a monoid. It
-- responds to 'pure' by returning a spline that immediately returns the
-- argument. It responds to '<*>' by applying the left arguments eventual
-- value (the function) to the right arguments eventual value. The
-- iteration values will me combined with 'mappend'.
instance (Monoid (f b), Applicative m, Monad m)
    => Applicative (SplineT m f a b) where
    pure = SplineTConst
    (SplineTConst f) <*> (SplineTConst x) = SplineTConst $ f x
    (SplineT vf) <*> (SplineTConst x) = SplineT $ fmap (fmap ($ x)) vf
    (SplineTConst f) <*> (SplineT vx) = SplineT $ fmap (fmap f) vx
    (SplineT vf) <*> (SplineT vx) = SplineT $ ((<*>) <$> vf) <*> vx

-- | A spline is monad if its iteration type is a monoid. A spline responds
-- to bind by running until it produces an eventual value, then uses that
-- value to run the next spline.
instance (Monoid (f b), Applicative m, Monad m) => Monad (SplineT m f a b) where
    (SplineTConst x) >>= f = f x
    (SplineT v) >>= f = SplineT $ Var $ \i -> do
        (Step b e, v') <- runVar v i
        case e of
            NoEvent -> return (Step b NoEvent, runSplineT $ SplineT v' >>= f)
            Event x -> runVar (runSplineT $ f x) i

-- | A spline can do IO if its underlying monad has a MonadIO instance. It
-- takes the result of the IO action as its immediate return value and
-- uses 'mempty' to generate an empty iteration value.
instance (Monoid (f b), MonadIO m) => MonadIO (SplineT m f a b) where
    liftIO f = SplineT $ Var $ \_ -> do
        n <- (Step mempty . Event) <$> liftIO f
        return (n, pure n)

-- | Evaluates a spline to a varying value of its iteration type.
execSplineT :: (Applicative m, Monad m, Monoid (f b))
            => SplineT m f a b c -> Var m a (f b)
execSplineT = (stepIter <$>) . runSplineT

-- | Evaluates a spline to an event stream of its result. The resulting
-- varying value inhibits until the spline's domain is complete and then it
-- produces events of the result type.
evalSplineT :: (Applicative m, Monad m, Monoid (f b))
            => SplineT m f a b c -> Var m a (Event c)
evalSplineT = (stepResult <$>) . runSplineT

-- | Create a spline using an event stream. The spline will run until the
-- stream inhibits, using the stream's last produced value as the current
-- iteration value. In the case the stream inhibits before producing
-- a value the default value is used. The spline's result value is the last
-- iteration value.
spline :: (Applicative m, Monad m) => x -> Var m a (Event x) -> Spline m a x x
spline x ve = SplineT $ Var $ \a -> do
    (ex, ve') <- runVar ve a
    case ex of
        NoEvent  -> let n = Step (Event x) (Event x) in return (n, pure n)
        Event x' -> return (Step (Event x') NoEvent, runSplineT $ spline x' ve')

-- | Create a spline from a varying value and an event stream. The spline
-- uses the varying value as its iteration value. The spline will run until
-- the event stream produces a value, at that point the last iteration
-- value and the event value are used in a merge function to produce the
-- spline's result value.
varyUntilEvent :: (Applicative m, Monad m)
               => Var m a b -> Var m a (Event c) -> (b -> c -> d)
               -> Spline m a b d
varyUntilEvent v ve f = SplineT $ Var $ \a -> do
    (b, v') <- runVar v a
    (ec, ve') <- runVar ve a
    case ec of
        NoEvent -> return (Step (Event b) NoEvent, runSplineT $ varyUntilEvent v' ve' f)
        Event c -> let n = Step (Event b) (Event $ f b c)
                   in return (n, pure n)
