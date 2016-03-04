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
    var,
    varM,
    mkState,
    -- * Composing value streams
    -- $composition
    (<<<),
    (>>>),
    -- * Adjusting and accumulating
    delay,
    accumulate,
    -- * Sampling value streams (running and other entry points)
    -- $running
    evalVar,
    execVar,
    loopVar,
    loopVar_,
    whileVar,
    whileVar_,
    scanVar,
    stepMany,
    -- * Testing value streams
    testVar,
    testVar_,
    testWhile_,
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
var f = VarT $ \a -> pure (f a, var f)

-- | Lift a monadic computation into a stream.
varM :: Monad m => (a -> m b) -> VarT m a b
varM f = VarT $ \a -> do
    b <- f a
    return (b, varM f)

-- | Create a stream from a state transformer.
mkState :: Monad m
        => (a -> s -> (b, s)) -- ^ state transformer
        -> s -- ^ intial state
        -> VarT m a b
mkState f s = VarT $ \a -> do
  let (b', s') = f a s
  return (b', mkState f s')
--------------------------------------------------------------------------------
-- $running
-- The easiest way to sample a stream is to run it in the desired monad with
-- 'runVarT'. This will produce a sample value and a new stream.
--
-- > do (sample, v') <- runVarT v inputValue
--
-- Much like Control.Monad.State there are other entry points for running
-- value streams like 'evalVar', 'execVar'. There are also extra control
-- structures such as 'loopVar' and 'whileVar'.
--------------------------------------------------------------------------------
-- | Iterate a stream once and return the sample value.
evalVar :: Functor m => VarT m a b -> a -> m b
evalVar v a = fst <$> runVarT v a

-- | Iterate a stream once and return the next stream.
execVar :: Functor m => VarT m a b -> a -> m (VarT m a b)
execVar v a = snd <$> runVarT v a

-- | Loop over a stream that takes no input value.
loopVar_ :: (Functor m, Monad m) => VarT m () a -> m ()
loopVar_ v = execVar v () >>= loopVar_

-- | Loop over a stream that produces its own next input value.
loopVar :: Monad m => a -> VarT m a a -> m a
loopVar a v = runVarT v a >>= uncurry loopVar

-- | Iterate a stream that requires no input until the given predicate fails.
whileVar_ :: Monad m => (a -> Bool) -> VarT m () a -> m a
whileVar_ f v = do
   (a, v') <- runVarT v ()
   if f a then whileVar_ f v' else return a

-- | Iterate a stream that produces its own next input value until the given
-- predicate fails.
whileVar :: Monad m
         => (a -> Bool) -- ^ The predicate to evaluate samples.
         -> a -- ^ The initial input/sample value.
         -> VarT m a a -- ^ The stream to iterate
         -> m a -- ^ The last sample
whileVar f a v = if f a
                 then runVarT v a >>= uncurry (whileVar f)
                 else return a

-- | Iterate a stream over a list of input until all input is consumed,
-- then iterate the stream using one single input. Returns the resulting
-- output value and the new stream.
stepMany :: (Monad m, Functor m) => [a] -> a -> VarT m a b -> m (b, VarT m a b)
stepMany [] e v = runVarT v e
stepMany (e:es) x v = execVar v e >>= stepMany es x

-- | Run the stream over the input values, gathering the output values in a
-- list.
scanVar :: (Applicative m, Monad m) => VarT m a b -> [a] -> m [b]
scanVar v = liftM snd . foldM f (v,[])
    where f (v', outs) a = do (b, v'') <- runVarT v' a
                              return (v'', outs ++ [b])
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

-- | A utility function for testing streams that don't require input. Runs
-- a stream printing each sample until the given predicate fails.
testWhile_ :: Show a => (a -> Bool) -> VarT IO () a -> IO ()
testWhile_ f v = do
    (a, v') <- runVarT v ()
    when (f a) $ print a >> testWhile_ f v'

-- | A utility function for testing streams that require input. The input
-- must have a 'Read' instance. Use this in GHCI to step through your streams
-- by typing the input and hitting `return`.
testVar :: (Read a, Show b) => VarT IO a b -> IO ()
testVar v = loopVar_ $ varM (const $ putStrLn "input: ")
                    >>> varM (const getLine)
                    >>> var read
                    >>> v
                    >>> varM print

-- | A utility function for testing streams that don't require input. Use
-- this in GHCI to step through your streams using the `return` key.
testVar_ :: Show b => VarT IO () b -> IO ()
testVar_ v = loopVar_ $ pure () >>> v >>> varM print >>> varM (const getLine)
--------------------------------------------------------------------------------
-- Adjusting and accumulating
--------------------------------------------------------------------------------
-- | Accumulates input values using a folding function and yields
-- that accumulated value each sample.
accumulate :: Monad m => (c -> b -> c) -> c -> VarT m b c
accumulate f b = VarT $ \a -> do
    let b' = f b a
    return (b', accumulate f b')

-- | Delays the given stream by one sample using the argument as the first
-- sample. This enables the programmer to create streams that depend on
-- themselves for values. For example:
--
-- > let v = 1 + delay 0 v in testVar_ v
delay :: Monad m => b -> VarT m a b -> VarT m a b
delay b v = VarT $ \a -> return (b, go a v)
    where go a v' = VarT $ \a' -> do (b', v'') <- runVarT v' a
                                     return (b', go a' v'')
--------------------------------------------------------------------------------
-- $composition
-- You can compose value streams together using Arrow's '>>>' and '<<<'. The
-- "right plug" ('>>>') takes the output from a value stream on the left and
-- "plugs" it into the input of the value stream on the right. The "left plug"
-- does the same thing in the opposite direction. This allows you to write value
-- streams that read naturally.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Typeclass instances
--------------------------------------------------------------------------------
-- | You can transform the sample value of any stream:
--
-- >  fmap (*3) $ accumulate (+) 0
-- Will sum input values and then multiply the sum by 3.
instance (Applicative m, Monad m) => Functor (VarT m b) where
    fmap f' v = v >>> var f'

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
    f0 . g0 = VarT $ \a -> do (b, g) <- runVarT g0 a
                              (c, f) <- runVarT f0 b
                              return (c, f . g)

-- | Streams are applicative.
--
-- >  (,) <$> pure True <*> var "Applicative"
instance (Applicative m, Monad m) => Applicative (VarT m a) where
    pure = var . const
    vf <*> va = VarT $ \a -> do (f, vf') <- runVarT vf a
                                (b, va') <- runVarT va a
                                return (f b, vf' <*> va')

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
data VarT m a b =
     VarT { runVarT :: a -> m (b, VarT m a b)
            -- ^ Given an input value, return a computation that effectfully
            -- produces an output value and a new stream for producing the next
            -- sample.
          }
