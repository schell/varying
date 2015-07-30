-- |
--   Module:     Control.Varying.Core
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell.scivally@synapsegroup.com>
--
--   Values that change over a given domain.
--
--   Varying values take some input (the domain ~ time, place, etc) and produce
--   a sample and a new varying value. This pattern is known as an automaton.
--   `varying` uses this pattern as its base type with the additon of a monadic
--   computation to create locally stateful signals that change over some
--   domain.
module Control.Varying.Core (
    Var(..),
    -- * Creating varying values
    -- $creation
    var,
    varM,
    -- * Composing varying values
    -- $composition
    (<~),
    (~>),
    -- * Adjusting and accumulating
    delay,
    accumulate,
    -- * Sampling varying values (running, entry points)
    -- $running
    evalVar,
    execVar,
    loopVar,
    loopVar_,
    whileVar,
    whileVar_,
    -- * Testing varying values
    testVar,
    testVar_,
    testWhile_,
    vtrace,
    vstrace
) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Applicative
import Debug.Trace
--------------------------------------------------------------------------------
-- $creation
-- You can create a pure varying value by lifting a function @(a -> b)@
-- with 'var':
--
-- @
-- addsOne :: Monad m => Var m Int Int
-- addsOne = var (+1)
-- @
--
-- 'var' is also equivalent to 'arr'.
--
-- You can create a monadic varying value by lifting a monadic computation
-- @(a -> m b)@ using 'varM':
--
-- @
-- getsFile :: Var IO FilePath String
-- getsFile = varM readFile
-- @
--
-- You can create either with the raw constructor. You can also create your
-- own combinators using the raw constructor, as it allows you full control
-- over how varying values are stepped and sampled:
--
-- @
-- delay :: Monad m => b -> Var m a b -> Var m a b
-- delay b v = Var $ \a -> return (b, go a v)
--     where go a v' = Var $ \a' -> do (b', v'') <- runVar v' a
--                                     return (b', go a' v'')
-- @
--
--------------------------------------------------------------------------------
-- | Lift a pure computation into a 'Var'.
var :: Applicative a => (b -> c) -> Var a b c
var f = Var $ \a -> pure $ (f a, var f)

-- | Lift a monadic computation into a 'Var'.
varM :: Monad m => (a -> m b) -> Var m a b
varM f = Var $ \a -> do
    b <- f a
    return (b, varM f)
--------------------------------------------------------------------------------
-- $running
-- The easiest way to sample a 'Var' is to run it in the desired monad with
-- 'runVar'. This will give you a sample value and a new 'Var' bundled up in a
-- tuple:
--
-- > do (sample, v') <- runVar v inputValue
--
-- Much like Control.Monad.State there are other entry points for running
-- varying values like 'evalVar', 'execVar'. There are also extra control
-- structures like 'loopVar' and 'whileVar' and more.
--------------------------------------------------------------------------------

-- | Iterate a 'Var' once and return the sample value.
evalVar :: Functor m => Var m a b -> a -> m b
evalVar v a = fst <$> (runVar v a)

-- | Iterate a 'Var' once and return the next 'Var'.
execVar :: Functor m => Var m a b -> a -> m (Var m a b)
execVar v a = snd <$> (runVar v a)

-- | Loop over a 'Var' that takes no input value.
loopVar_ :: Monad m => Var m () a -> m ()
loopVar_ v = execVar v () >>= loopVar_

-- | Loop over a 'Var' that produces its own next input value.
loopVar :: Monad m => a -> Var m a a -> m a
loopVar a v = runVar v a >>= uncurry loopVar

-- | Iterate a 'Var' that requires no input until the given predicate fails.
whileVar_ :: Monad m => (a -> Bool) -> Var m () a -> m a
whileVar_ f v = do
   (a, v') <- runVar v ()
   if f a then whileVar_ f v' else return a

-- | Iterate a 'Var' that produces its own next input value until the given
-- predicate fails.
whileVar :: Monad m
         => (a -> Bool) -- ^ The predicate to evaluate samples.
         -> a -- ^ The initial input/sample value.
         -> Var m a a -- ^ The 'Var' to iterate
         -> m a -- ^ The last sample
whileVar f a v = if f a
                 then runVar v a >>= uncurry (whileVar f)
                 else return a
--------------------------------------------------------------------------------
-- Testing and debugging
--------------------------------------------------------------------------------
-- | Trace the sample value of a 'Var' and pass it along as output. This is
-- very useful for debugging graphs of 'Var's.
vtrace :: (Applicative a, Show b) => Var a b b
vtrace = vstrace ""

-- | Trace the sample value of a 'Var' with a prefix and pass the sample along
-- as output. This is very useful for debugging graphs of 'Var's.
vstrace :: (Applicative a, Show b) => String -> Var a b b
vstrace s = var $ \b -> trace (s ++ show b) b

-- | A utility function for testing 'Var's that don't require input. Runs
-- a 'Var' printing each sample until the given predicate fails.
testWhile_ :: Show a => (a -> Bool) -> Var IO () a -> IO ()
testWhile_ f v = do
    (a, v') <- runVar v ()
    if f a then print a >> testWhile_ f v' else return ()

-- | A utility function for testing 'Var's that require input. The input
-- must have a 'Read' instance. Use this in GHCI to step through your 'Var's
-- by typing the input and hitting `return`.
testVar :: (Read a, Show b) => Var IO a b -> IO ()
testVar v = loopVar_ $ varM (const $ putStrLn "input: ")
                    ~> varM (const getLine)
                    ~> var read
                    ~> v
                    ~> varM (putStrLn . show)

-- | A utility function for testing 'Var's that don't require input. Use
-- this in GHCI to step through your 'Var's using the `return` key.
testVar_ :: Show b => Var IO () b -> IO ()
testVar_ v = loopVar_ $ pure () ~> v ~> varM print ~> varM (const $ getLine)
--------------------------------------------------------------------------------
-- Adjusting and accumulating
--------------------------------------------------------------------------------
-- | Accumulates input values using a folding function and yields
-- that accumulated value each sample.
accumulate :: Monad m => (c -> b -> c) -> c -> Var m b c
accumulate f b = Var $ \a -> do
    let b' = f b a
    return (b', accumulate f b')

-- | Delays the given 'Var' by one sample using a parameter as the first
-- sample. This enables the programmer to create 'Var's that depend on
-- themselves for values. For example:
--
-- > let v = 1 + delay 0 v in testVar_ v
delay :: Monad m => b -> Var m a b -> Var m a b
delay b v = Var $ \a -> return (b, go a v)
    where go a v' = Var $ \a' -> do (b', v'') <- runVar v' a
                                    return (b', go a' v'')
--------------------------------------------------------------------------------
-- $composition
-- You can compose varying values together using '~>' and '<~'. The "right plug"
-- ('~>') takes the output from a varying value on the left and "plugs" it
-- into the input of the varying value on the right. The "left plug" does
-- the same thing only in the opposite direction. This allows you to write
-- varying values that read naturally.
--------------------------------------------------------------------------------
-- | Same as '~>' with flipped parameters.
(<~) :: Monad m => Var m b c -> Var m a b -> Var m a c
(<~) = flip (~>)
infixl 1 <~

-- | Connects two 'Var's by chaining the first's output into the input of the
-- second. This is the defacto 'Var' composition method and in fact '.' is an
-- alias of '<~', which is just '~>' flipped.
(~>) :: Monad m => Var m a b -> Var m b c -> Var m a c
(~>) v1 v2 = Var $ \a -> do
    (b, v1') <- runVar v1 a
    (c, v2') <- runVar v2 b
    return $ (c, v1' ~> v2')
infixr 1 ~>
--------------------------------------------------------------------------------
-- Typeclass instances
--------------------------------------------------------------------------------
-- | You can transform the sample value of any 'Var':
--
-- >  fmap (*3) $ accumulate (+) 0
-- Will sum input values and then multiply the sum by 3.
instance Monad m => Functor (Var m b) where
    fmap f' v = v ~> var f'

-- | A very simple category instance.
--
-- @
--   id = var id
--   f . g = g ~> f
-- @
-- or
--
-- >  f . g = f <~ g
--
-- It is preferable for consistency (and readability) to use 'plug left' ('<~')
-- and 'plug right' ('~>') instead of ('.') where possible.
instance Monad m => Category (Var m) where
    id = var id
    f . g = g ~> f

-- | 'Var's are applicative.
--
-- >  (,) <$> pure True <*> var "Applicative"
instance Monad m => Applicative (Var m a) where
    pure = var . const
    vf <*> va = Var $ \a -> do (f, vf') <- runVar vf a
                               (b, va') <- runVar va a
                               return $ (f b, vf' <*> va')

-- | 'Var's are arrows, which means you can use proc notation.
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
instance Monad m => Arrow (Var m) where
    arr = var
    first v = Var $ \(b,d) -> do (c, v') <- runVar v b
                                 return $ ((c,d), first v')

-- | 'Var's can be written as numbers.
--
-- >  let v = 1 ~> accumulate (+) 0
-- which will sum the natural numbers.
instance (Monad m, Num b) => Num (Var m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

-- | 'Var's can be written as floats.
--
-- >  let v = pi ~> accumulate (*) 0.0
-- which will attempt (and succeed) to multiply pi by zero every step.
instance (Monad m, Floating b) => Floating (Var m a b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

-- | 'Var's can be written as fractionals.
--
-- >  let v = 2.5 ~> accumulate (+) 0
-- which will add 2.5 each step.
instance (Monad m, Fractional b) => Fractional (Var m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational
--------------------------------------------------------------------------------
-- Core datatypes
--------------------------------------------------------------------------------
-- | The vessel of a varying value. A 'Var' is a structure that contains a value
-- that changes over some input. That input could be time (Float, Double, etc)
-- or 'Control.Varying.Event.Event's or 'Char' - whatever.
-- It's a kind of Mealy machine (an automaton) with effects.
data Var m b c =
     Var { runVar :: b -> m (c, Var m b c)
                  -- ^ Given an input value, return a computation that
                  -- effectfully produces an output value (a sample) and a 'Var'
                  -- for producing the next sample.
         }
