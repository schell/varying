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
    execSpline,
    spline,
    -- * Spline Transformer
    SplineT(..),
    runSplineT,
    evalSplineT,
    execSplineT,
    output,
    -- * Special operations.
    untilEvent,
    race,
    mix,
    capture,
    mapOutput,
    -- * Step
    Step(..),
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad
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

toIter :: (Functor f, Monoid (f b))
         => (f a -> f b) -> Step (f a) c -> Step (f b) c
toIter f (Step a b) = Step (f a) b

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

-- | Unwrap a spline into a value stream.
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
    return = pure
    (SplineTConst x) >>= f = f x
    (SplineT v) >>= f = SplineT $ Var $ \i -> do
        (Step b e, v') <- runVar v i
        case e of
            NoEvent -> return (Step b NoEvent, runSplineT $ SplineT v' >>= f)
            Event x -> runVar (runSplineT $ f x) i

-- | A spline is a transformer and other monadic computations can be lifted
-- int a spline.
instance Monoid (f b) => MonadTrans (SplineT f a b) where
    lift f = SplineT $ varM $ const $ liftM (Step mempty . Event) f

-- | A spline can do IO if its underlying monad has a MonadIO instance. It
-- takes the result of the IO action as its immediate return value and
-- uses 'mempty' to generate an empty output value.
instance (Monoid (f b), Functor m, Applicative m, MonadIO m)
    => MonadIO (SplineT f a b m) where
    liftIO = lift . liftIO

-- | Evaluates a spline to a value stream of its output type.
execSplineT :: (Applicative m, Monad m, Monoid (f b))
            => SplineT f a b m c -> Var m a (f b)
execSplineT = (stepIter <$>) . runSplineT

-- | Evaluates a spline to an event stream of its result. The resulting
-- value stream inhibits until the spline's domain is complete and then it
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

-- | Using a default start value, evaluate the spline to a value stream.
-- A spline is only defined over a finite domain so we must supply a default
-- value to use before the spline produces its first output value.
execSpline :: (Applicative m, Monad m) => b -> Spline a b m c -> Var m a b
execSpline x (SplineTConst _) = pure x
execSpline x s = execSplineT s ~> foldStream (\_ y -> y) x

-- | Create a spline from a value stream and an event stream. The spline
-- uses the value stream as its output value. The spline will run until
-- the event stream produces a value, at that point the last output
-- value and the event value are tupled and returned as the spline's result
-- value.
untilEvent :: (Applicative m, Monad m)
           => Var m a b -> Var m a (Event c)
           -> Spline a b m (b,c)
untilEvent v ve = SplineT $ t ~> var (uncurry f)
    where t = (,) <$> v <*> ve
          f b ec = case ec of
                       NoEvent -> Step (Event b) NoEvent
                       Event c -> Step (Event b) (Event (b, c))

-- | Run two splines concurrently and return the result of the SplineT that
-- concludes first. If they conclude at the same time the result is taken from
-- the spline on the left.
race :: (Applicative m, Monad m, Monoid (f u))
          => SplineT f i u m a -> SplineT f i u m a -> SplineT f i u m a
race (SplineTConst a) s =
    race (SplineT $ pure $ Step mempty $ Event a) s
race s (SplineTConst b) =
    race s (SplineT $ pure $ Step mempty $ Event b)
race (SplineT va) (SplineT vb) = SplineT $ Var $ \i -> do
    (Step ua ea, va') <- runVar va i
    (Step ub eb, vb') <- runVar vb i
    case (ea,eb) of
        (Event _,_) -> return (Step (ua <> ub) ea, va')
        (_,Event _) -> return (Step (ua <> ub) eb, vb')
        (_,_)       -> return (Step (ua <> ub) NoEvent,
                               runSplineT $ race (SplineT va') (SplineT vb'))

-- | Run a list of splines concurrently. Restart individual splines whenever
-- they conclude in a value. Return a list of the most recent result values once
-- the control spline concludes.
mix :: (Applicative m, Monad m, Monoid (f b))
    => [Maybe c -> SplineT f a b m c] -> SplineT f a b m ()
    -> SplineT f a b m [Maybe c]
mix gs = go gs es $ zipWith ($) gs xs
    where es = replicate n NoEvent
          xs = replicate n Nothing
          n  = length gs
          go fs evs guis egui = SplineT $ Var $ \a -> do
            let step (ecs, fb, vs) (f, ec, g) = do
                    (Step fb' ec', v) <- runVar (runSplineT g) a
                    let ec'' = ec <> ec'
                        fb'' = fb <> fb'
                        v'   = case ec' of
                                   NoEvent -> v
                                   Event c -> runSplineT $ f $ Just c
                    return (ecs ++ [ec''], fb'', vs ++ [SplineT v'])
            (ecs, fb, guis') <- foldM step ([],mempty,[]) (zip3 fs evs guis)
            (Step fb' ec, v) <- runVar (runSplineT egui) a
            let fb'' = fb <> fb'
                ec' = map toMaybe ecs <$ ec
            return (Step fb'' ec',
                    runSplineT $ go fs ecs guis' $ SplineT v)

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

-- | Produce the argument as an output value exactly once, then return ().
output :: (Applicative m, Monad m, Monoid (f b), Applicative f)
       => b -> SplineT f a b m ()
output b = SplineT $ Var $ \_ ->
    return (Step (pure b) NoEvent, pure $ Step (pure b) $ Event ())

-- | Map the output value of a spline.
mapOutput :: (Functor f, Monoid (f t), Applicative m, Monad m)
          => Var m a (b -> t) -> SplineT f a b m c -> SplineT f a t m c
mapOutput _ (SplineTConst c) = SplineTConst c
mapOutput vf (SplineT vx) = SplineT $ toIter <$> vg <*> vx
    where vg = (<$>) <$> vf
