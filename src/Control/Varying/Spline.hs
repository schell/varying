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
    fromEvents,
    outputStream,
    resultStream,
    step,
    -- * Combinators
    untilEvent,
    untilEvent_,
    _untilEvent,
    _untilEvent_,
    pair,
    race,
    capture,
    mapOutput,
    adjustInput,
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
import Data.Functor.Identity

-- | A discrete step in a continuous function. This type discretely describes
-- an eventual value on the right and an output value on the left.
data Step b c = Step { stepOutput :: b
                     , stepResult :: Event c
                     }

-- | Map the output value of a 'Step'.
mapStepOutput :: (a -> b) -> Step a c -> Step b c
mapStepOutput f (Step a b) = Step (f a) b

-- | A discrete step is a functor by applying a function to the contained
-- event's value.
instance Functor (Step a) where
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

-- | 'SplineT' shares a number of types with 'VarT', specifically its monad,
-- input and output types (@m@, @a@ and @b@, respectively). A spline adds
-- a result type which represents the monadic computation's result
-- value.
-- Much like the State monad it has an "internal state" and an eventual
-- result value, where the internal state is the output value. The result
-- value is used only in determining the next spline to sequence.
data SplineT a b m c = SplineT { unSplineT :: VarT m a (Step (Event b) c) }
                     | SplineTConst c

-- | Unwrap a spline into a value stream.
runSplineT :: (Applicative m, Monad m)
           => SplineT a b m c -> VarT m a (Step (Event b) c)
runSplineT (SplineT v) = v
runSplineT (SplineTConst x) = pure $ pure x

-- | Run the spline over the input values, gathering the output and result
-- values in a list.
scanSpline :: (Applicative m, Monad m)
           => SplineT a b m c -> [a] -> m [(Event b, Event c)]
scanSpline s as = map f <$> scanVar (runSplineT s) as
    where f (Step eb ec) = (eb,ec)

-- | A SplineT monad parameterized with Identity that takes input of type @a@,
-- output of type @b@ and a result value of type @c@.
type Spline a b c = SplineT a b Identity c

-- | A spline is a functor by applying the function to the result.
instance (Applicative m, Monad m) => Functor (SplineT a b m) where
    fmap f (SplineTConst c)  = SplineTConst $ f c
    fmap f (SplineT v) = SplineT $ fmap (fmap f) v

-- | A spline is an applicative if its output type is a monoid. It
-- responds to 'pure' by returning a spline that immediately returns the
-- argument. It responds to '<*>' by applying the left arguments eventual
-- value (the function) to the right arguments eventual value. The
-- output values will me combined with 'mappend'.
instance (Applicative m, Monad m) => Applicative (SplineT a b m) where
    pure = SplineTConst
    (SplineTConst f) <*> (SplineTConst x) = SplineTConst $ f x
    (SplineT vf) <*> (SplineTConst x) = SplineT $ fmap (fmap ($ x)) vf
    (SplineTConst f) <*> (SplineT vx) = SplineT $ fmap (fmap f) vx
    (SplineT vf) <*> (SplineT vx) = SplineT $ liftA2 (<*>) vf vx

-- | A spline is monad if its output type is a monoid. A spline responds
-- to bind by running until it produces an eventual value, then uses that
-- value to run the next spline.
instance (Applicative m, Monad m) => Monad (SplineT a b m) where
    return = pure
    (SplineTConst x) >>= f = f x
    (SplineT v) >>= f = SplineT $ VarT $ \i -> do
        (Step b e, v') <- runVarT v i
        case e of
            NoEvent -> return (Step b NoEvent, runSplineT $ SplineT v' >>= f)
            Event x -> runVarT (runSplineT $ f x) i

-- | A spline is a transformer and other monadic computations can be lifted
-- int a spline.
instance MonadTrans (SplineT a b) where
    lift f = SplineT $ varM $ const $ fmap (Step mempty . Event) f

-- | A spline can do IO if its underlying monad has a MonadIO instance. It
-- takes the result of the IO action as its immediate return value and
-- uses 'mempty' to generate an empty output value.
instance (Functor m, Applicative m, MonadIO m) => MonadIO (SplineT a b m) where
    liftIO = lift . liftIO

-- | Evaluates a spline into a value stream of its output type.
outputStream :: (Applicative m, Monad m)
             => b -> SplineT a b m c -> VarT m a b
outputStream x s = ((stepOutput <$>) $ runSplineT s) ~> foldStream (\_ y -> y) x

-- | Evaluates a spline to an event stream of its result. The resulting
-- value stream inhibits until the spline's domain is complete and then it
-- produces events of the result type.
resultStream :: (Applicative m, Monad m) => SplineT a b m c -> VarT m a (Event c)
resultStream = (stepResult <$>) . runSplineT

-- | Create a spline using an event stream. The spline will run until the
-- stream inhibits, using the stream's last produced value as the current
-- output value. In the case the stream inhibits before producing
-- a value the default value is used. The spline's result is the last
-- output value.
fromEvents :: (Applicative m, Monad m) => b -> VarT m a (Event b) -> SplineT a b m b
fromEvents x ve = SplineT $ VarT $ \a -> do
    (ex, ve') <- runVarT ve a
    case ex of
        NoEvent  -> let n = Step (Event x) (Event x) in return (n, pure n)
        Event x' -> return ( Step (Event x') NoEvent
                           , runSplineT $ fromEvents x' ve'
                           )

-- | Create a spline from a value stream and an event stream. The spline
-- uses the value stream as its output value. The spline will run until
-- the event stream produces a value, at that point the last output
-- value and the event value are tupled and returned as the spline's result
-- value.
untilEvent :: (Applicative m, Monad m)
           => VarT m a b -> VarT m a (Event c)
           -> SplineT a b m (b,c)
untilEvent v ve = SplineT $ t ~> var (uncurry f)
    where t = (,) <$> v <*> ve
          f b ec = case ec of
                       NoEvent -> Step (Event b) NoEvent
                       Event c -> Step (Event b) (Event (b, c))

-- | A variant of 'untilEvent' that only results in the left result,
-- discarding the right result.
untilEvent_ :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c)
            -> SplineT a b m b
untilEvent_ v ve = fst <$> untilEvent v ve

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
_untilEvent_ v ve = void $ untilEvent v ve


-- | Run two splines in parallel, combining their output. Return the result of
-- the spline that concludes first. If they conclude at the same time the result
-- is taken from the left spline.
race :: (Applicative m, Monad m)
     => (b -> d -> e) -> SplineT a b m c -> SplineT a d m e -> SplineT a e m (Either c e)
race f (SplineTConst a) s =
    race f (SplineT $ pure $ Step mempty $ Event a) s
race f s (SplineTConst b) =
    race f s (SplineT $ pure $ Step mempty $ Event b)
race f (SplineT va) (SplineT vb) = SplineT $ VarT $ \i -> do
    (Step ua ea, va') <- runVarT va i
    (Step ub eb, vb') <- runVarT vb i
    let s' = runSplineT $ race f (SplineT va') (SplineT vb')
    case (ea,eb) of
        (Event _,_) -> return (Step (f <$> ua <*> ub) $ Left <$> ea, s')
        (_,Event _) -> return (Step (f <$> ua <*> ub) $ Right <$> eb, s')
        (_,_)       -> return (Step (f <$> ua <*> ub) NoEvent, s')

-- | Run two splines in parallel, combining their output.  When both conclude,
-- return their result values in a tuple.
pair :: (Monad m)
     => (b -> d -> f) -> SplineT a b m c -> SplineT a d m e
     -> SplineT a f m (c, e)
pair f (SplineTConst a) s = pair f (SplineT $ pure $ Step mempty $ Event a) s
pair f s (SplineTConst b) = pair f s (SplineT $ pure $ Step mempty $ Event b)
pair f (SplineT va) (SplineT vb) = SplineT $ VarT $ \a -> do
    (Step fa ea, va') <- runVarT va a
    (Step fb eb, vb') <- runVarT vb a
    return ( Step (f <$> fa <*> fb) ((,) <$> ea <*> eb)
           , runSplineT $ pair f (SplineT va') (SplineT vb')
           )

-- | Capture the spline's last output value and tuple it with the
-- spline's result. This is helpful when you want to sample the last
-- output value in order to determine the next spline to sequence.
capture :: (Applicative m, Monad m, Eq b)
        => SplineT a b m c -> SplineT a b m (Maybe b, c)
capture (SplineTConst x) = SplineTConst (Nothing, x)
capture (SplineT v) = capture' Nothing v
    where capture' mb v' = SplineT $ VarT $ \a -> do
              (Step fb ec, v'') <- runVarT v' a
              let mb' = if fb == NoEvent then mb else toMaybe fb
                  ec' = (mb',) <$> ec
              return (Step fb ec', runSplineT $ capture' mb' v'')

-- | Produce the argument as an output value exactly once.
step :: (Applicative m, Monad m) => b -> SplineT a b m ()
step b = SplineT $ VarT $ \_ ->
    return (Step (pure b) NoEvent, pure $ Step (pure b) $ Event ())

-- | Map the output value of a spline.
mapOutput :: (Applicative m, Monad m)
          => VarT m a (b -> t) -> SplineT a b m c -> SplineT a t m c
mapOutput _ (SplineTConst c) = SplineTConst c
mapOutput vf (SplineT vx) = SplineT $ mapStepOutput <$> vg <*> vx
    where vg = (<$>) <$> vf

-- | Map the input value of a spline.
adjustInput :: (Monad m)
            => VarT m a (a -> r) -> SplineT r b m c -> SplineT a b m c
adjustInput _ (SplineTConst c) = SplineTConst c
adjustInput vf (SplineT vx) = SplineT $ VarT $ \a -> do
    (f, vf') <- runVarT vf a
    (b, vx') <- runVarT vx $ f a
    return (b, runSplineT $ adjustInput vf' $ SplineT vx')
