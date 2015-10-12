-- |
--   Module:     Control.Varying.SplineT
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  Using splines we can easily create continuously varying values from
--  multiple piecewise event streams. A spline is a monadic layer on top of
--  event streams which are only continuous over a certain domain. The idea
--  is that we use do notation to "run an event stream" from which we will
--  consume produced values. Once the event stream inhibits the do-notation
--  computation completes and returns a result value. That result value is then
--  used to determine the next spline in the sequence. This allows us to build
--  up long, complex behaviors sequentially using a very familiar notation
--  that can be easily turned into a continuously varying value.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Control.Varying.Spline (
    -- * Spline
    Spline,
    runSpline,
    execSpline,
    spline,
    -- * Spline Transformer
    SplineT(..),
    runSplineT,
    evalSplineT,
    execSplineT,
    varyUntilEvent,
    capture,
    -- * Step
    Step(..),
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Monoid

-- | A discrete step in a continuous function. This is simply a type that
-- discretely describes an eventual value on the right and a monoidal output
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

-- | 'SplineT' shares a number of types with 'Var', specifically its monad,
-- input and output types (m, a and b, respectively). A spline adds
-- a container type that determines how empty output values should be
-- created, appended and applied (the type must be monoidal and applicative).
-- It also adds a result type which represents the monadic computation's result
-- value.
-- Much like the State monad it has an "internal state" and an eventual
-- return value, where the internal state is the output value. The result
-- value is used only in determining the next spline to sequence.
data SplineT f a b m c = SplineT { unSplineT :: Var m a (Step (f b) c) }
                       | SplineTConst c

-- | Unwrap a spline into a varying value.
runSplineT :: (Applicative m, Monad m, Monoid (f b))
           => SplineT f a b m c -> Var m a (Step (f b) c)
runSplineT (SplineT v) = v
runSplineT (SplineTConst x) = pure $ pure x

-- | 'Spline' is a specialized 'SplineT' that uses Event as its output
-- container. This means that new values overwrite/replace old values due to
-- Event's 'Last'-like monoid instance.
type Spline a b m c = SplineT Event a b m c

-- | A spline is a functor by applying the function to the result.
instance (Applicative m, Monad m) => Functor (SplineT f a b m) where
    fmap f (SplineTConst c)  = SplineTConst $ f c
    fmap f (SplineT v) = SplineT $ fmap (fmap f) v

-- | A spline is an applicative if its output type is a monoid. It
-- responds to 'pure' by returning a spline that immediately returns the
-- argument. It responds to '<*>' by applying the left arguments eventual
-- value (the function) to the right arguments eventual value. The
-- output values will me combined with 'mappend'.
instance (Monoid (f b), Applicative m, Monad m)
    => Applicative (SplineT f a b m) where
    pure = SplineTConst
    (SplineTConst f) <*> (SplineTConst x) = SplineTConst $ f x
    (SplineT vf) <*> (SplineTConst x) = SplineT $ fmap (fmap ($ x)) vf
    (SplineTConst f) <*> (SplineT vx) = SplineT $ fmap (fmap f) vx
    (SplineT vf) <*> (SplineT vx) = SplineT $ ((<*>) <$> vf) <*> vx

-- | A spline is monad if its output type is a monoid. A spline responds
-- to bind by running until it produces an eventual value, then uses that
-- value to run the next spline.
instance (Monoid (f b), Applicative m, Monad m) => Monad (SplineT f a b m) where
    (SplineTConst x) >>= f = f x
    (SplineT v) >>= f = SplineT $ Var $ \i -> do
        (Step b e, v') <- runVar v i
        case e of
            NoEvent -> return (Step b NoEvent, runSplineT $ SplineT v' >>= f)
            Event x -> runVar (runSplineT $ f x) i

instance Monoid (f b) => MonadTrans (SplineT f a b) where
    lift f = SplineT $ Var $ \_ -> do
        n <- (Step mempty . Event) <$> f
        return (n, pure n)

-- | A spline can do IO if its underlying monad has a MonadIO instance. It
-- takes the result of the IO action as its immediate return value and
-- uses 'mempty' to generate an empty output value.
instance (Monoid (f b), Functor m, Applicative m, MonadIO m)
    => MonadIO (SplineT f a b m) where
    liftIO = lift . liftIO

-- | Evaluates a spline to a varying value of its output type.
execSplineT :: (Applicative m, Monad m, Monoid (f b))
            => SplineT f a b m c -> Var m a (f b)
execSplineT = (stepIter <$>) . runSplineT

-- | Evaluates a spline to an event stream of its result. The resulting
-- varying value inhibits until the spline's domain is complete and then it
-- produces events of the result type.
evalSplineT :: (Applicative m, Monad m, Monoid (f b))
            => SplineT f a b m c -> Var m a (Event c)
evalSplineT = (stepResult <$>) . runSplineT

-- | Create a spline using an event stream. The spline will run until the
-- stream inhibits, using the stream's last produced value as the current
-- output value. In the case the stream inhibits before producing
-- a value the default value is used. The spline's result value is the last
-- output value.
spline :: (Applicative m, Monad m) => b -> Var m a (Event b) -> Spline a b m b
spline x ve = SplineT $ Var $ \a -> do
    (ex, ve') <- runVar ve a
    case ex of
        NoEvent  -> let n = Step (Event x) (Event x) in return (n, pure n)
        Event x' -> return (Step (Event x') NoEvent, runSplineT $ spline x' ve')

-- | Unwrap a spline into a varying value. This is an alias of
-- 'runSplineT'.
runSpline :: (Applicative m, Monad m) => Spline a b m c -> Var m a (Step (Event b) c)
runSpline = runSplineT

-- | Using a default start value, evaluate the spline to a varying value.
-- A spline is only defined over a finite domain so we must supply a default
-- value to use before the spline produces its first output value.
execSpline :: (Applicative m, Monad m) => b -> Spline a b m c -> Var m a b
execSpline x (SplineTConst _) = pure x
execSpline x s = execSplineT s ~> foldStream (\_ y -> y) x

-- | Create a spline from a varying value and an event stream. The spline
-- uses the varying value as its output value. The spline will run until
-- the event stream produces a value, at that point the last output
-- value and the event value are used in a merge function to produce the
-- spline's result value.
varyUntilEvent :: (Applicative m, Monad m)
               => Var m a b -> Var m a (Event c) -> (b -> c -> d)
               -> Spline a b m d
varyUntilEvent v ve f = SplineT $ Var $ \a -> do
    (b, v') <- runVar v a
    (ec, ve') <- runVar ve a
    case ec of
        NoEvent -> return (Step (Event b) NoEvent,
                           runSplineT $ varyUntilEvent v' ve' f)
        Event c -> let n = Step (Event b) (Event $ f b c)
                   in return (n, pure n)

-- | Capture the spline's latest output value and tuple it with the
-- spline's result value. This is helpful when you want to sample the last
-- output value in order to determine the next spline to sequence.
capture :: (Applicative m, Monad m, Monoid (f b), Eq (f b))
        => SplineT f a b m c -> SplineT f a b m (f b, c)
capture (SplineTConst x) = SplineTConst (mempty, x)
capture (SplineT v) = capture' mempty v
    where capture' mb v' = SplineT $ Var $ \a -> do
              (Step fb ec, v'') <- runVar v' a
              let mb' = if fb == mempty then mb else fb
                  ec' = (mb',) <$> ec
              return (Step fb ec', runSplineT $ capture' mb' v'')
