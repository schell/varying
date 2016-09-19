{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
-- |
--   Module:     Control.Varying.Core
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   Value streams represent values that change over a given domain.
--
--   A stream takes some input (the domain e.g. time, place, etc) and when
--   sampled using 'runVarT' - produces a value and a new value stream. This
--   pattern is known as an automaton. `varying` uses this pattern as its base
--   type with the additon of a monadic computation to create locally stateful
--   signals that change over some domain.
module Control.Varying.Core (
    Var,
    VarT(..),
    -- * Creating value streams
    -- $creation
    done,
    var,
    varM,
    mkState,
    -- * Composing value streams
    -- $composition
    (<~),
    (~>),
    (<<<),
    (>>>),
    -- * Adjusting and accumulating
    delay,
    accumulate,
    -- * Sampling value streams (running and other entry points)
    -- $running
    scanVar,
    stepMany,
    -- * Tracing value streams in flight
    vtrace,
    vstrace,
    vftrace,
) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Functor.Identity
import Debug.Trace
--------------------------------------------------------------------------------
-- Core datatypes
--------------------------------------------------------------------------------
-- | A value stream parameterized with Identity that takes input of type @a@
-- and gives output of type @b@. This is the pure, effect-free version of
-- 'VarT'.
type Var a b = VarT Identity a b

-- | A value stream is a structure that contains a value that changes over some
-- input. It's a kind of Mealy machine (an automaton) with effects. Using
-- 'runVarT' with an input value of type 'a' yields a "step", which is a value
-- of type 'b' and a new 'VarT' for yielding the next value.
newtype VarT m a b = VarT { runVarT :: a -> m (b, VarT m a b) }
                  -- ^ Given an input value, return a computation that
                  -- effectfully produces an output value and a new stream for
                  -- producing the next sample.
--------------------------------------------------------------------------------
-- $creation
-- You can create a pure value stream by lifting a function @(a -> b)@
-- with 'var':
--
-- @
-- addsOne :: Monad m => VarT m Int Int
-- addsOne = var (+1)
-- @
--
-- 'var' is equivalent to 'arr'.
--
-- You can create a monadic value stream by lifting a monadic computation
-- @(a -> m b)@ using 'varM':
--
-- @
-- getsFile :: VarT IO FilePath String
-- getsFile = varM readFile
-- @
--
-- You can create either with the raw constructor. You can also create your
-- own combinators using the raw constructor, as it allows you full control
-- over how value streams are stepped and sampled:
--
-- @
-- delay :: Monad m => b -> VarT m a b -> VarT m a b
-- delay b v = VarT $ \a -> return (b, go a v)
--     where go a v' = VarT $ \a' -> do (b', v'') <- runVarT v' a
--                                     return (b', go a' v'')
-- @
--
--------------------------------------------------------------------------------
-- | Lift a pure computation into a stream.
var :: Applicative m => (a -> b) -> VarT m a b
var f = VarT $ \(!a) -> pure (f a, var f)

-- | Lift a constant value into a stream.
done :: (Applicative m, Monad m) => b -> VarT m a b
done b = VarT $ \(!_) -> return (b, done b)

-- | Lift a monadic computation into a stream.
varM :: Monad m => (a -> m b) -> VarT m a b
varM f = VarT $ \(!a) -> do
    b <- f a
    return (b, varM f)

-- | Create a stream from a state transformer.
mkState :: Monad m
        => (a -> s -> (b, s)) -- ^ state transformer
        -> s -- ^ intial state
        -> VarT m a b
mkState f s = VarT $ \(!a) -> do
  let (b', s') = f a s
  return (b', mkState f s')
--------------------------------------------------------------------------------
-- $running
-- To sample a stream simply run it in the desired monad with
-- 'runVarT'. This will produce a sample value and a new stream.
--
-- > do (sample, v') <- runVarT v inputValue

--------------------------------------------------------------------------------
-- | Iterate a stream over a list of input until all input is consumed,
-- then iterate the stream using one single input. Returns the resulting
-- output value and the new stream.
stepMany :: (Monad m, Functor m) => VarT m a b -> [a] -> a -> m (b, VarT m a b)
stepMany v [] e = runVarT v e
stepMany v (e:es) x = snd <$> runVarT v e >>= \v1 -> stepMany v1 es x

-- | Run the stream over the input values, gathering the output values in a
-- list.
scanVar :: (Applicative m, Monad m) => VarT m a b -> [a] -> m ([b], VarT m a b)
scanVar v = foldM f ([], v)
    where f (outs, v') a = do (b, v'') <- runVarT v' a
                              return (outs ++ [b], v'')
--------------------------------------------------------------------------------
-- Testing and debugging
--------------------------------------------------------------------------------
-- | Trace the sample value of a stream and pass it along as output. This is
-- very useful for debugging graphs of streams.
vtrace :: (Applicative a, Show b) => VarT a b b
vtrace = vstrace ""

-- | Trace the sample value of a stream with a prefix and pass the sample along
-- as output. This is very useful for debugging graphs of streams.
vstrace :: (Applicative a, Show b) => String -> VarT a b b
vstrace s = vftrace ((s ++) . show)

-- | Trace the sample value after being run through a "show" function.
-- This is very useful for debugging graphs of streams.
vftrace :: Applicative a => (b -> String) -> VarT a b b
vftrace f = var $ \b -> trace (f b) b
--------------------------------------------------------------------------------
-- Adjusting and accumulating
--------------------------------------------------------------------------------
-- | Accumulates input values using a folding function and yields
-- that accumulated value each sample.
accumulate :: (Monad m, Applicative m) => (c -> b -> c) -> c -> VarT m b c
accumulate f b = VarT $ \(!a) -> do
    let b' = f b a
    return (b', accumulate f b')

-- | Delays the given stream by one sample using the argument as the first
-- sample. This enables the programmer to create streams that depend on
-- themselves for values. For example:
--
-- > let v = 1 + delay 0 v in testVar_ v
delay :: (Monad m, Applicative m) => b -> VarT m a b -> VarT m a b
delay b v = VarT $ \(!a) -> return (b, go a v)
    where go a v' = VarT $ \(!a') -> do (b', v'') <- runVarT v' a
                                        return (b', go a' v'')
--------------------------------------------------------------------------------
-- $composition
-- You can compose value streams together using Arrow's '>>>' and '<<<' or the
-- synonyms '~>' and '<~'. The "right plug" ('>>>' and '~>') takes the output
-- from a value stream on the left and "plugs" it into the input of the value
-- stream on the right.
-- The "left plug" does the same thing in the opposite direction. This allows
-- you to write value streams that read naturally.
--------------------------------------------------------------------------------
(~>) :: (Monad m, Applicative m) => VarT m a b -> VarT m b c -> VarT m a c
(~>) = (>>>)

(<~) :: (Monad m, Applicative m) => VarT m b c -> VarT m a b -> VarT m a c
(<~) = (<<<)
--------------------------------------------------------------------------------
-- Typeclass instances
--------------------------------------------------------------------------------
-- | You can transform the sample value of any stream:
--
-- >  fmap (*3) $ accumulate (+) 0
-- Will sum input values and then multiply the sum by 3.
instance (Applicative m, Monad m) => Functor (VarT m b) where
  fmap f v = v >>> var f
-- | A very simple category instance.
--
-- @
--   id = var id
--   f . g = g >>> f
-- @
-- or
--
-- >  f . g = f <<< g
--
-- It is preferable for consistency (and readability) to use 'plug left' ('<<<')
-- and 'plug right' ('>>>') instead of ('.') where possible.
instance (Applicative m, Monad m) => Category (VarT m) where
    id = var id
    f0 . g0 = VarT $ \(!a) -> do
      (b, g) <- runVarT g0 a
      (c, f) <- runVarT f0 b
      return (c, f . g)

-- | Streams are applicative.
--
-- >  (,) <$> pure True <*> var "Applicative"
instance (Applicative m, Monad m) => Applicative (VarT m a) where
    pure = done
    vf <*> vx = VarT $ \(!a) -> do
      (f, vf') <- runVarT vf a
      (x, vx') <- runVarT vx a
      return (f x, vf' <*> vx')
-- Note [1]

-- | Streams are arrows, which means you can use proc notation.
--
-- @
-- v = proc a -> do
--       ex <- intEventVar -< ()
--       ey <- anotherIntEventVar -< ()
--       returnA -\< (+) \<$\> ex \<*\> ey
-- @
-- which is equivalent to
--
-- >  v = (\ex ey -> (+) <$> ex <*> ey) <$> intEventVar <*> anotherIntEventVar
instance (Applicative m, Monad m) => Arrow (VarT m) where
    arr = var
    first v = VarT $ \(b,d) -> do (c, v') <- runVarT v b
                                  return ((c,d), first v')

-- | Streams can be monoids
--
-- > let v = var (const "Hello ") `mappend` var (const "World!")
instance (Applicative m, Monad m, Monoid b) => Monoid (VarT m a b) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- | Streams can be written as numbers.
--
-- >  let v = 1 >>> accumulate (+) 0
-- which will sum the natural numbers.
instance (Applicative m, Monad m, Num b) => Num (VarT m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

-- | Streams can be written as floats.
--
-- >  let v = pi >>> accumulate (*) 0.0
-- which will attempt (and succeed) to multiply pi by zero every step.
instance (Applicative m, Monad m, Floating b) => Floating (VarT m a b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

-- | Streams can be written as fractionals.
--
-- >  let v = 2.5 >>> accumulate (+) 0
-- which will add 2.5 each step.
instance (Applicative m, Monad m, Fractional b) => Fractional (VarT m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational


-- [1] Proof of the applicative laws:
--
-- identity
-- ========
-- pure id <*> va = va
--
-- -- Definition of pure
-- VarT (\_ -> pure (id, pure id)) <*> v
--
-- -- Definition of <*>
-- VarT (\x -> do
--   (f, vf') <- runVarT (VarT (\_ -> pure (id, pure id))) x
--   (a, va') <- runVarT va x
--   pure (f a, vf' <*> va'))
--
-- -- Newtype
-- VarT (\x -> do
--   (f, vf') <- (\_ -> pure (id, pure id)) x
--   (a, va') <- runVarT va x
--   pure (f a, vf' <*> va'))
--
-- -- Application
-- VarT (\x -> do
--   (f, vf') <- pure (id, pure id)
--   (a, va') <- runVarT va x
--   pure (f a, vf' <*> va'))
--
-- -- pure x >>= f = f x
-- VarT (\x -> do
--   (a, va') <- runVarT va x
--   pure (id a, pure id <*> va'))
--
-- -- Definition of id
-- VarT (\x -> do
--   (a, va') <- runVarT va x
--   pure (a, pure id <*> va'))
--
-- -- Coinduction
-- VarT (\x -> do
--   (a, va') <- runVarT va x
--   pure (a, va'))
--
-- -- f >>= pure = f
-- VarT (\x -> runVarT va x)
--
-- -- Eta reduction
-- VarT (runVarT va)
--
-- -- Newtype
-- va
--
--
-- composition
-- ===========
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- -- Definition of pure
-- VarT (\_ -> pure ((.), pure (.))) <*> u <*> v <*> w
--
-- -- Definition of <*>
-- VarT (\x -> do
--   (h, t)  <- runVarT (VarT (\_ -> pure ((.), pure (.)))) x
--   (f, u') <- runVarT u x
--   pure (h f, t <*> u')) <*> v <*> w
--
-- -- Newtype
-- VarT (\x -> do
--   (h, t)  <- (\_ -> pure ((.), pure (.))) x
--   (f, u') <- runVarT u x
--   pure (h f, t <*> u')) <*> v <*> w
--
-- -- Application
-- VarT (\x -> do
--   (h, t)  <- pure ((.), pure (.)))
--   (f, u') <- runVarT u x
--   pure (h f, t <*> u')) <*> v <*> w
--
-- -- pure x >>= f = f x
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   pure ((.) f, pure (.) <*> u')) <*> v <*> w
--
-- -- Definition of <*>
-- VarT (\x -> do
--   (h, t) <-
--     runVarT
--       (VarT (\y -> do
--         (f, u') <- runVarT u y
--         pure ((.) f, pure (.) <*> u'))) x
--   (g, v') <- runVarT v x
--   pure (h g, t <*> v')) <*> w
--
-- -- Newtype
-- VarT (\x -> do
--   (h, t) <-
--     (\y -> do
--       (f, u') <- runVarT u y
--       pure ((.) f, pure (.) <*> u')) x
--   (g, v') <- runVarT v x
--   pure (h g, t <*> v')) <*> w
--
-- -- Application
-- VarT (\x -> do
--   (h, t) <- do
--     (f, u') <- runVarT u x
--     pure ((.) f, pure (.) <*> u')
--   (g, v') <- runVarT v x
--   pure (h g, t <*> v')) <*> w
--
-- -- (f >=> g) >=> h = f >=> (g >=> h)
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (h, t)  <- pure ((.) f, pure (.) <*> u')
--   (g, v') <- runVarT v x
--   pure (h g, t <*> v')) <*> w
--
-- -- pure x >>= f = f x
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (g, v') <- runVarT v x
--   pure ((.) f g, pure (.) <*> u' <*> v')) <*> w
--
-- -- Definition of <*>
-- VarT (\x -> do
--   (h, t) <-
--     runVarT
--       (VarT (\y -> do
--         (f, u') <- runVarT u y
--         (g, v') <- runVarT v y
--         pure ((.) f g, pure (.) <*> u' <*> v'))) x
--   (a, w') <- runVarT w x
--   pure (h a, t <*> w'))
--
-- -- Newtype
-- VarT (\x -> do
--   (h, t) <-
--     (\y -> do
--       (f, u') <- runVarT u y
--       (g, v') <- runVarT v y
--       pure ((.) f g, pure (.) <*> u' <*> v')) x
--   (a, w') <- runVarT w x
--   pure (h a, t <*> w'))
--
-- -- Application
-- VarT (\x -> do
--   (h, t) <- do
--     (f, u') <- runVarT u x
--     (g, v') <- runVarT v x
--     pure ((.) f g, pure (.) <*> u' <*> v'))
--   (a, w') <- runVarT w x
--   pure (h a, t <*> w'))
--
-- -- (f >=> g) >=> h = f >=> (g >=> h)
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (g, v') <- runVarT v x
--   (h, t)  <- pure ((.) f g, pure (.) <*> u' <*> v'))
--   (a, w') <- runVarT w x
--   pure (h a, t <*> w'))
--
-- -- pure x >>= f = f x
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (g, v') <- runVarT v x
--   (a, w') <- runVarT w x
--   pure ((.) f g a, pure (.) <*> u' <*> v' <*> w'))
--
-- -- Definition of .
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (g, v') <- runVarT v x
--   (a, w') <- runVarT w x
--   pure (f (g a), pure (.) <*> u' <*> v' <*> w'))
--
-- -- Coinduction
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (g, v') <- runVarT v x
--   (a, w') <- runVarT w x
--   pure (f (g a), u' <*> (v' <*> w')))
--
-- -- pure x >>= f = f
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (g, v') <- runVarT v x
--   (a, w') <- runVarT w x
--   (b, vw) <- pure (g a, v' <*> w')
--   pure (f b, u' <*> vw))
--
-- -- (f >=> g) >=> h = f >=> (g >=> h)
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (b, vw) <- do
--     (g, v') <- runVarT v x
--     (a, w') <- runVarT w x
--     pure (g a, v' <*> w')
--   pure (f b, u' <*> vw))
--
-- -- Abstraction
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (b, vw) <-
--     (\y -> do
--       (g, v') <- runVarT v y
--       (a, w') <- runVarT w y)
--       pure (g a, v' <*> w')) x
--   pure (f b, u' <*> vw))
--
-- -- Newtype
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (b, vw) <-
--     runVarT
--       (VarT (\y -> do
--         (g, v') <- runVarT v y
--         (a, w') <- runVarT w y)
--         pure (g a, v' <*> w')) x
--   pure (f b, u' <*> vw))
--
-- -- Definition of <*>
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (b, vw) <- runVarT (v <*> w) x
--   pure (f b, u' <*> vw))
--
-- -- Definition of <*>
-- u <*> (v <*> w)
--
--
-- homomorphism
-- ============
-- pure f <*> pure a = pure (f a)
--
-- -- Definition of pure
-- VarT (\_ -> pure (f, pure f)) <*> pure a
--
-- -- Definition of pure
-- VarT (\_ -> pure (f, pure f)) <*> VarT (\_ -> pure (a, pure a))
--
-- -- Definition of <*>
-- VarT (\x -> do
--   (f', vf') <- runVarT (VarT (\_ -> pure (f, pure f))) x
--   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
--   pure (f' a', vf' <*> va'))
--
-- -- Newtype
-- VarT (\x -> do
--   (f', vf') <- (\_ -> pure (f, pure f)) x
--   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
--   pure (f' a', vf' <*> va'))
--
-- -- Application
-- VarT (\x -> do
--   (f', vf') <- pure (f, pure f)
--   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
--   pure (f' a', vf' <*> va'))
--
-- -- pure x >>= f = f x
-- VarT (\x -> do
--   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
--   pure (f a', pure f <*> va'))
--
-- -- Newtype
-- VarT (\x -> do
--   (a', va') <- (\_ -> pure (a, pure a)) x
--   pure (f a', pure f <*> va'))
--
-- -- Application
-- VarT (\x -> do
--   (a', va') <- pure (a, pure a)
--   pure (f a', pure f <*> va'))
--
-- -- pure x >>= f = f x
-- VarT (\x -> pure (f a, pure f <*> pure a))
--
-- -- Coinduction
-- VarT (\x -> pure (f a, pure (f a)))
--
-- -- Definition of pure
-- pure (f a)
--
--
-- interchange
-- ===========
-- u <*> pure y = pure ($ y) <*> u
--
-- -- Definition of <*>
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (a, y') <- runVarT (pure y) x
--   pure (f a, u' <*> y'))
--
-- -- Definition of pure
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (a, y') <- runVarT (VarT (\_ -> pure (y, pure y))) x
--   pure (f a, u' <*> y'))
--
-- -- Newtype
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (a, y') <- (\_ -> pure (y, pure y)) x
--   pure (f a, u' <*> y'))
--
-- -- Application
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   (a, y') <- pure (y, pure y))
--   pure (f a, u' <*> y'))
--
-- -- pure x >>= f = f
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   pure (f y, u' <*> pure y))
--
-- -- Coinduction
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   pure (f y, pure ($ y) <*> u'))
--
-- -- Definition of $
-- VarT (\x -> do
--   (f, u') <- runVarT u x
--   pure (($ y) f, pure ($ y) <*> u')
--
-- -- pure x >>= f = f
-- VarT (\x -> do
--   (g, y') <- pure (($ y), pure ($ y))
--   (f, u') <- runVarT u x
--   pure (g f, y' <*> u')
--
-- -- Abstraction
-- VarT (\x -> do
--   (g, y') <- (\_ -> pure (($ y), pure ($ y))) x
--   (f, u') <- runVarT u x
--   pure (g f, y' <*> u')
--
-- -- Newtype
-- VarT (\x -> do
--   (g, y') <- runVarT (VarT (\_ -> pure (($ y), pure ($ y)))) x
--   (f, u') <- runVarT u x
--   pure (g f, y' <*> u')
--
-- -- Definition of <*>
-- VarT (\_ -> pure (($ y), pure ($ y))) <*> u
--
-- -- Definition of pure
-- pure ($ y) <*> u
