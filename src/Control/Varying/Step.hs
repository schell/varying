-- |
--   Module:     Control.Varying.Step
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--  Using 'Step' we can easily create continuous varying values from
--  multiple piecewise varying values.
--

module Control.Varying.Step (
    Step,
    runStep,
    step
) where

import Control.Varying.Core
import Control.Varying.Event

data Step m a b c = Step { unStep :: Var m a (b, Event c) }
                  | End c

-- | A Step is a functor by applying the function to the result.
instance Monad m => Functor (Step m a b) where
    fmap f (Step v) = Step $ fmap (fmap (fmap f)) v
    fmap f (End c)  = End $ f c

instance Monad m => Applicative (Step m a b) where
    pure = End
    (End f) <*> (End x) = End $ f x
    (Step vf) <*> (End x) = Step $ fmap (fmap (fmap ($ x))) vf
    (End f) <*> (Step vx) = Step $ fmap (fmap (fmap f)) vx
    (Step vf) <*> (Step vx) = Step $ Var $ \i -> do
        ((_, ef), vf') <- runVar vf i
        ((b, ex), vx') <- runVar vx i
        return ((b, ef <*> ex), unStep $ (Step vf') <*> (Step vx'))

instance (Monad m, Monoid b) => Monad (Step m a b) where
    (End x) >>= f = f x
    (Step v) >>= f = Step $ Var $ \i -> do
        ((b, e), v') <- runVar v i
        case e of
            NoEvent -> return ((b, NoEvent), unStep $ Step v' >>= f)
            Event x -> runVar (unStep $ f x) i

runStep :: Monad m => Step m a b c -> Var m a b
runStep = (fst <$>) . unStep

step :: Monad m
     => Var m a b -> Var m a (Event c) -> (b -> c -> d) -> Step m a b d
step v ve f = Step $ Var $ \a -> do
    (b, v') <- runVar v a
    (ec, ve') <- runVar ve a
    case ec of
        NoEvent -> return ((b,NoEvent), unStep $ step v' ve' f)
        Event c -> return ((b,Event $ f b c), pure (b,Event $ f b c))
