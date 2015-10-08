-- |
--   Module:     Control.Varying.Spline
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  Using 'Spline' we can easily create continuous varying values from
--  multiple piecewise varying values.
--

module Control.Varying.Spline (
    Spline(..),
    evalSpline,
    execSpline,
    spline,
    varyUntilEvent,
    varyUntilEvent_
) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad.IO.Class

-- | A discrete step in a continuous function. This is simply a type that
-- discretely describes an eventual value on the right and an iteration
-- value on the left.
data Step b c = Step { stepIter  :: b
                     , stepEvent :: (Event c)
                     } deriving (Show, Eq)

-- | A discrete step is a functor by applying a function to the contained
-- event's value.
instance Functor (Step b) where
    fmap f (Step b c) = Step b $ fmap f c

-- | A discrete spline is a monoid if its left and right types are monoids.
instance (Monoid a, Monoid b) => Monoid (Step a b) where
    mempty = Step mempty (Event mempty)
    mappend (Step a ea) (Step b eb) = Step (mappend a b) (mappend <$> ea <*> eb)

-- | A discrete spline is an applicative if its left datatype is a monoid. It
-- replies to 'pure' with an empty left value while the right value is the
-- argument wrapped in an event. It means "the argument happens instantly".
instance Monoid a => Applicative (Step a) where
    pure a = Step mempty $ Event a
    (Step uia f) <*> (Step uib b) = Step (mappend uia uib) (f <*> b)

-- | A spline is a varying value that is continuous over a domain. A spline
-- can end in a result value of a different type than its iteration type.
-- Much like the State monad it has an "internal state" and an eventual
-- return type. In most cases the internal state (the iteration value) is
-- the interesting part of the spline, but sometimes it's useful to use the
-- return value in determining the next spline.
data Spline m a b c = Spline { runSpline :: Var m a (Step b c) }
                    | SplineConst c

-- | A spline is a functor by applying the function to the result.
instance Monad m => Functor (Spline m a b) where
    fmap f (Spline v) = Spline $ fmap (fmap f) v
    fmap f (SplineConst c)  = SplineConst $ f c

-- | A spline is an applicative if its iteration type is a monoid. It
-- responds to 'pure' by returning a spline that immediately returns the
-- argument. It responds to '<*>' by applying the left arguments eventual
-- value (the function) to the right arguments eventual value. The
-- iteration values will me combined with 'mappend'.
instance (Monad m, Monoid b) => Applicative (Spline m a b) where
    pure = SplineConst
    (SplineConst f) <*> (SplineConst x) = SplineConst $ f x
    (Spline vf) <*> (SplineConst x) = Spline $ fmap (fmap ($ x)) vf
    (SplineConst f) <*> (Spline vx) = Spline $ fmap (fmap f) vx
    (Spline vf) <*> (Spline vx) = Spline $ ((<*>) <$> vf) <*> vx
-- if we want to get rid of the (Monoid b) constraint...
--    (Spline vf) <*> (Spline vx) = Spline $ Var $ \i -> do
--        (Step _ ef, vf') <- runVar vf i
--        (Step b ex, vx') <- runVar vx i
--        return (Step b (ef <*> ex), runSpline $ (Spline vf') <*> (Spline vx'))

-- | A spline is monad if its iteration type is a monoid. A spline responds
-- to bind by running until it produces an eventual value, then uses that
-- value to run the next spline.
instance (Monad m, Monoid b) => Monad (Spline m a b) where
    (SplineConst x) >>= f = f x
    (Spline v) >>= f = Spline $ Var $ \i -> do
        (Step b e, v') <- runVar v i
        case e of
            NoEvent -> return (Step b NoEvent, runSpline $ Spline v' >>= f)
            Event x -> runVar (runSpline $ f x) i

instance (MonadIO m, Monoid b) => MonadIO (Spline m a b) where
    liftIO f = Spline $ Var $ \_ -> do
        c <- liftIO f
        let n = Step mempty (Event c)
        return (n, pure n)

execSpline :: Monad m => Spline m a b c -> Var m a b
execSpline = (stepIter <$>) . runSpline

evalSpline :: Monad m => Spline m a b c -> Var m a (Event c)
evalSpline = (stepEvent <$>) . runSpline

spline :: Monad m => x -> Var m a (Event x) -> Spline m a x x
spline x ve = Spline $ Var $ \a -> do
    (ex, ve') <- runVar ve a
    case ex of
        NoEvent  -> let n = Step x (Event x) in return (n, pure n)
        Event x' -> return (Step x' NoEvent, runSpline $ spline x' ve')

varyUntilEvent :: Monad m
               => Var m a b -> Var m a (Event c) -> (b -> c -> d)
               -> Spline m a b d
varyUntilEvent v ve f = Spline $ Var $ \a -> do
    (b, v') <- runVar v a
    (ec, ve') <- runVar ve a
    case ec of
        NoEvent -> return (Step b NoEvent, runSpline $ varyUntilEvent v' ve' f)
        Event c -> let n = Step b (Event $ f b c)
                   in return (n, pure n)

varyUntilEvent_ :: (Monad m, Monoid b)
                => Var m a b -> Var m a (Event c) -> (b -> c -> d)
                -> Spline m a b ()
varyUntilEvent_ v ve f = varyUntilEvent v ve f >> return ()

