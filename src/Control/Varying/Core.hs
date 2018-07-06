{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ > 710
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
--   Module:     Control.Varying.Core
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <schell@takt.com>
--
--   Varying values represent values that change over a given domain.
--
--   A varying value takes some input as its domain (e.g. time, place, etc)
--   and when run using 'runVarT' it produces a value and a new varying value.
--   This pattern is known as an automaton and `varying` uses this pattern at its
--   core. With the additon of monadic event sequencing, 'varying' makes it easy
--   to construct complicated signals that control program and data flow.
module Control.Varying.Core
  ( -- * Types and Typeclasses
    Var
  , VarT(..)
    -- * Creating vars
    -- $creation
  , done
  , var
  , arr
  , varM
  , mkState
    -- * Composing vars
    -- $composition
  , (<<<)
  , (>>>)
    -- * Adjusting and accumulating
  , delay
  , accumulate
    -- * Sampling vars (running and other entry points)
    -- $running
  , scanVar
  , stepMany
    -- * Debugging and tracing vars in flight
  , vtrace
  , vstrace
  , vftrace
  , testVarOver
    -- * Proofs of the Applicative laws
    -- $proofs
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import           Debug.Trace
import           Prelude                    hiding (id, (.))
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif
--------------------------------------------------------------------------------
-- Core datatypes
--------------------------------------------------------------------------------
-- | A continuously varying value, with effects.
-- It's a kind of <https://en.wikipedia.org/wiki/Mealy_machine Mealy machine>
-- (an automaton).
newtype VarT m a b = VarT { runVarT :: a -> m (b, VarT m a b) }
                            -- ^ Run a @VarT@ computation with an input value of
                            -- type 'a', yielding a step - a value of type 'b'
                            -- and a new computation for yielding the next step.


-- | A var parameterized with Identity that takes input of type @a@
-- and gives output of type @b@. This is the pure, effect-free version of
-- 'VarT'.
type Var a b = VarT Identity a b

--------------------------------------------------------------------------------
-- Typeclass instances
--------------------------------------------------------------------------------
-- | You can transform the output value of any var:
--
-- >>> let v = 1 >>> fmap (*3) (accumulate (+) 0)
-- >>> testVarOver v [(),(),()]
-- 3
-- 6
-- 9
instance Applicative m => Functor (VarT m b) where
  fmap f v = VarT $ (g <$>) . runVarT v
    where g (b, vb) = (f b, f <$> vb)

-- | A var is a category.
-- @
--   id = var id
--   f . g = g >>> f
-- @
--
-- or
--
-- >  f . g = f <<< g
--
-- >>> let v = accumulate (+) 0 . 1
-- >>> testVarOver v [(),(),()]
-- 1
-- 2
-- 3
instance Monad m => Category (VarT m) where
    id = var id
    f0 . g0 = VarT $ \a -> do
      (b, g) <- runVarT g0 a
      (c, f) <- runVarT f0 b
      return (c, f . g)

-- | Vars are applicative.
--
-- >>> let v = (,) <$> pure True <*> pure "Applicative"
-- >>> testVarOver v [()]
-- (True,"Applicative")
--
-- Note - checkout the <$proofs proofs>
instance Applicative m => Applicative (VarT m a) where
    pure = done
    vf <*> vx = VarT $ \a ->
      g <$> runVarT vf a <*> runVarT vx a
      where g (f, vf1) (x, vx1) = (f x, vf1 <*> vx1)

-- | Vars are arrows, which means you can use proc notation, among other
-- meanings.
--
-- >>> :set -XArrows
-- >>> :{
-- let v = proc t -> do
--           x <- accumulate (+) 0 -< t
--           y <- accumulate (+) 1 -< t
--           returnA -< x + y
-- in testVarOver v [1,1,1]
-- >>> :}
-- 3
-- 5
-- 7
--
-- which is equivalent to
--
-- >>> let v = (+) <$> accumulate (+) 0 <*> accumulate (+) 1
-- >>> testVarOver v [1,1,1]
-- 3
-- 5
-- 7
instance Monad m => Arrow (VarT m) where
  arr = var
  first v = VarT $ \(b, d) -> g d <$> runVarT v b
    where g d (c, v') = ((c, d), first v')

instance MonadPlus m => ArrowZero (VarT m) where
  zeroArrow = varM $ const mzero

instance MonadPlus m => ArrowPlus (VarT m) where
  VarT f <+> VarT g = VarT $ \a -> f a `mplus` g a

-- |
instance Monad m => ArrowChoice (VarT m) where
  left f  = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  f ||| g = VarT $ \case
    Left b -> do
      (d, f1) <- runVarT f b
      return (d, f1 ||| g)
    Right c -> do
      (d, g1) <- runVarT g c
      return (d, f ||| g1)

instance Monad m => ArrowApply (VarT m) where
  app = VarT $ \(v, b) -> do
    (c, _) <- runVarT v b
    return (c, app)

instance MonadFix m => ArrowLoop (VarT m) where
  loop vmbdcd = VarT $ \b -> fmap fst $ mfix $ \(_, d) -> do
    ((c1, d1), vmbdcd1) <- runVarT vmbdcd (b, d)
    return ((c1, loop vmbdcd1), d1)

-- | VarT with its input and output parameters flipped.
newtype FlipVarT m b a = FlipVarT { unFlipVarT :: VarT m a b }

-- | A VarT is contravariant when the type arguments are flipped.
instance Monad m => Contravariant (FlipVarT m b) where
  contramap f (FlipVarT vmab) = FlipVarT $ VarT $ \c -> do
    (b, vmab1) <- runVarT vmab $ f c
    return (b, unFlipVarT $ contramap f $ FlipVarT vmab1)

#if __GLASGOW_HASKELL__ >= 840
-- | Vars can be semigroups
--
-- >>> let v = var (const "Hello ") <> var (const "World!")
-- >>> testVarOver v [()]
-- "Hello World!"
instance (Applicative m, Semigroup b) => Semigroup (VarT m a b) where
  (<>) = liftA2 (<>)
#endif

-- | Vars can be monoids
--
-- >>> let v = var (const "Hello ") `mappend` var (const "World!")
-- >>> testVarOver v [()]
-- "Hello World!"
instance (Applicative m, Monoid b) => Monoid (VarT m a b) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | Vars can be written as numbers.
--
-- >>> let v = 1 >>> accumulate (+) 0
-- >>> testVarOver v [(),(),()]
-- 1
-- 2
-- 3
instance (Monad m, Num b) => Num (VarT m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

-- | Vars can be written as floats.
--
-- >>> let v = pi >>> accumulate (*) 1 >>> arr round
-- >>> testVarOver v [(),(),()]
-- 3
-- 10
-- 31
instance (Monad m, Floating b) => Floating (VarT m a b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

-- | Vars can be written as fractionals.
--
-- >>> let v = 2.5 >>> accumulate (/) 10
-- >>> testVarOver v [(),(),()]
-- 4.0
-- 1.6
-- 0.64
instance (Monad m, Fractional b) => Fractional (VarT m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational
--------------------------------------------------------------------------------
-- $creation
-- You can create a pure var by lifting a function @(a -> b)@
-- with 'var':
--
-- > arr (+1) == var (+1) :: VarT m Int Int
--
-- 'var' is a parameterized version of 'arr'.
--
-- You can create a monadic var by lifting a monadic computation
-- @(a -> m b)@ using 'varM':
--
-- @
-- getsFile :: VarT IO FilePath String
-- getsFile = varM readFile
-- @
--
-- You can create either with the raw constructor. You can also create your
-- own combinators using the raw constructor, as it allows you full control
-- over how vars are stepped and sampled:
--
-- > delay :: Monad m => b -> VarT m a b -> VarT m a b
-- > delay b v = VarT $ \a -> return (b, go a v)
-- >     where go a v' = VarT $ \a' -> do (b', v'') <- runVarT v' a
-- >                                      return (b', go a' v'')
-- >
--------------------------------------------------------------------------------
-- | Lift a pure computation to a var. This is 'arr' parameterized over the
-- @a `VarT m` b@ arrow.
var :: Applicative m => (a -> b) -> VarT m a b
var f = VarT $ \a -> pure (f a, var f)

-- | Lift a monadic computation to a var. This is
-- <http://hackage.haskell.org/package/arrow-list-0.7/docs/Control-Arrow-Kleisli-Class.html#v:arrM arrM>
-- parameterized over the @a `VarT m` b@ arrow.
varM :: Monad m => (a -> m b) -> VarT m a b
varM f = VarT $ \a -> do
    b <- f a
    return (b, varM f)

-- | Lift a constant value to a var.
done :: Applicative m => b -> VarT m a b
done = var . const

-- | Create a var from a state transformer.
mkState :: Monad m
        => (a -> s -> (b, s)) -- ^ state transformer
        -> s -- ^ intial state
        -> VarT m a b
mkState f s = VarT $ \a -> do
  let (b', s') = f a s
  return (b', mkState f s')
--------------------------------------------------------------------------------
-- $composition
-- You can compose vars together using Category's '>>>' and '<<<'. The "right
-- plug" ('>>>') takes the output from a var on the left and "plugs" it into
-- the input of the var on the right. The "left plug" does the same thing in
-- the opposite direction. This allows you to write vars that read
-- naturally.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Adjusting and accumulating
--------------------------------------------------------------------------------
-- | Accumulates input values using a folding function and yields
-- that accumulated value each sample. This is analogous to a stepwise foldl.
--
-- >>> testVarOver (accumulate (++) []) $ words "hey there man"
-- "hey"
-- "heythere"
-- "heythereman"
--
-- >>> print $ foldl (++) [] $ words "hey there man"
-- "heythereman"
accumulate :: Monad m => (c -> b -> c) -> c -> VarT m b c
accumulate f b = VarT $ \a -> do
    let b' = f b a
    return (b', accumulate f b')

-- | Delays the given var by one sample using the argument as the first
-- sample.
--
-- >>> testVarOver (delay 0 id) [1,2,3]
-- 0
-- 1
-- 2
--
-- This enables the programmer to create vars that depend on
-- themselves for values. For example:
--
-- >>> let v = delay 0 v + 1 in testVarOver v [1,1,1]
-- 1
-- 2
-- 3
delay :: Monad m => b -> VarT m a b -> VarT m a b
delay b v = VarT $ \a -> return (b, go a v)
    where go a v' = VarT $ \a' -> do (b', v'') <- runVarT v' a
                                     return (b', go a' v'')
--------------------------------------------------------------------------------
-- $running
-- To sample a var simply run it in the desired monad with
-- 'runVarT'. This will produce a sample value and a new var.
--
-- >>> :{
-- do let v0 = accumulate (+) 0
--    (b, v1) <- runVarT v0 1
--    print b
--    (c, v2) <- runVarT v1 b
--    print c
--    (d,  _) <- runVarT v2 c
--    print d
-- >>> :}
-- 1
-- 2
-- 4
--------------------------------------------------------------------------------
-- | Iterate a var over a list of input until all input is consumed,
-- then iterate the var using one single input. Returns the resulting
-- output value and the new var.
--
-- >>> let Identity (outputs, _) = stepMany (accumulate (+) 0) [1,1,1] 1
-- >>> print outputs
-- 4
stepMany :: (Monad m) => VarT m a b -> [a] -> a -> m (b, VarT m a b)
stepMany v [] e     = runVarT v e
stepMany v (e:es) x = snd <$> runVarT v e >>= \v1 -> stepMany v1 es x

-- | Run the var over the input values, gathering the output values in a
-- list.
--
-- >>> let Identity (outputs, _) = scanVar (accumulate (+) 0) [1,1,1,1]
-- >>> print outputs
-- [1,2,3,4]
scanVar :: Monad m => VarT m a b -> [a] -> m ([b], VarT m a b)
scanVar v = foldM f ([], v)
    where f (outs, v') a = do (b, v'') <- runVarT v' a
                              return (outs ++ [b], v'')
--------------------------------------------------------------------------------
-- Testing and debugging
--------------------------------------------------------------------------------
-- | Trace the sample value of a var and pass it along as output. This is
-- very useful for debugging graphs of vars. The (v|vs|vf)trace family of
-- vars use 'Debug.Trace.trace' under the hood, so the value is only traced
-- when evaluated.
--
-- >>> let v = id >>> vtrace
-- >>> testVarOver v [1,2,3]
-- 1
-- 1
-- 2
-- 2
-- 3
-- 3
vtrace :: (Applicative a, Show b) => VarT a b b
vtrace = vstrace ""


-- | Trace the sample value of a var with a prefix and pass the sample along
-- as output. This is very useful for debugging graphs of vars.
--
-- >>> let v = id >>> vstrace "test: "
-- >>> testVarOver v [1,2,3]
-- test: 1
-- 1
-- test: 2
-- 2
-- test: 3
-- 3
vstrace :: (Applicative a, Show b) => String -> VarT a b b
vstrace s = vftrace ((s ++) . show)

-- | Trace the sample value using a custom show-like function. This is useful
-- when you would like to debug a var that uses values that don't have show
-- instances.
--
-- >>> newtype NotShowableInt = NotShowableInt { unNotShowableInt :: Int }
-- >>> let v = id >>> vftrace (("NotShowableInt: " ++) . show . unNotShowableInt)
-- >>> let as = map NotShowableInt [1,1,1]
-- >>> bs <- fst <$> scanVar v as
-- >>> -- We need to do something to evaluate these output values...
-- >>> print $ sum $ map unNotShowableInt bs
-- NotShowableInt: 1
-- NotShowableInt: 1
-- NotShowableInt: 1
-- 3
vftrace :: Applicative a => (b -> String) -> VarT a b b
vftrace f = var $ \b -> trace (f b) b

-- | Run a var in IO over some input, printing the output each step. This is
-- the function we've been using throughout this documentation.
testVarOver :: (Monad m, MonadIO m, Show b)
            => VarT m a b -> [a] -> m ()
testVarOver v xs = fst <$> scanVar v xs >>= mapM_ (liftIO . print)
--------------------------------------------------------------------------------
-- $proofs
-- ==Identity
-- > pure id <*> va = va
--
-- > -- Definition of pure
-- > VarT (\_ -> pure (id, pure id)) <*> v
--
-- > -- Definition of <*>
-- > VarT (\x -> do
-- >   (f, vf') <- runVarT (VarT (\_ -> pure (id, pure id))) x
-- >   (a, va') <- runVarT va x
-- >   pure (f a, vf' <*> va'))
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (f, vf') <- (\_ -> pure (id, pure id)) x
-- >   (a, va') <- runVarT va x
-- >   pure (f a, vf' <*> va'))
--
-- > -- Application
-- > VarT (\x -> do
-- >   (f, vf') <- pure (id, pure id)
-- >   (a, va') <- runVarT va x
-- >   pure (f a, vf' <*> va'))
--
-- > -- pure x >>= f = f x
-- > VarT (\x -> do
-- >   (a, va') <- runVarT va x
-- >   pure (id a, pure id <*> va'))
--
-- > -- Definition of id
-- > VarT (\x -> do
-- >   (a, va') <- runVarT va x
-- >   pure (a, pure id <*> va'))
--
-- > -- Coinduction
-- > VarT (\x -> do
-- >   (a, va') <- runVarT va x
-- >   pure (a, va'))
--
-- > -- f >>= pure = f
-- > VarT (\x -> runVarT va x)
--
-- > -- Eta reduction
-- > VarT (runVarT va)
--
-- > -- Newtype
-- > va
-- >
--
-- ==Composition
-- > pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- > -- Definition of pure
-- > VarT (\_ -> pure ((.), pure (.))) <*> u <*> v <*> w
--
-- > -- Definition of <*>
-- > VarT (\x -> do
-- >   (h, t)  <- runVarT (VarT (\_ -> pure ((.), pure (.)))) x
-- >   (f, u') <- runVarT u x
-- >   pure (h f, t <*> u')) <*> v <*> w
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (h, t)  <- (\_ -> pure ((.), pure (.))) x
-- >   (f, u') <- runVarT u x
-- >   pure (h f, t <*> u')) <*> v <*> w
--
-- > -- Application
-- > VarT (\x -> do
-- >   (h, t)  <- pure ((.), pure (.)))
-- >   (f, u') <- runVarT u x
-- >   pure (h f, t <*> u')) <*> v <*> w
--
-- > -- pure x >>= f = f x
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   pure ((.) f, pure (.) <*> u')) <*> v <*> w
--
-- > -- Definition of <*>
-- > VarT (\x -> do
-- >   (h, t) <-
-- >     runVarT
-- >       (VarT (\y -> do
-- >         (f, u') <- runVarT u y
-- >         pure ((.) f, pure (.) <*> u'))) x
-- >   (g, v') <- runVarT v x
-- >   pure (h g, t <*> v')) <*> w
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (h, t) <-
-- >     (\y -> do
-- >       (f, u') <- runVarT u y
-- >       pure ((.) f, pure (.) <*> u')) x
-- >   (g, v') <- runVarT v x
-- >   pure (h g, t <*> v')) <*> w
--
-- > -- Application
-- > VarT (\x -> do
-- >   (h, t) <- do
-- >     (f, u') <- runVarT u x
-- >     pure ((.) f, pure (.) <*> u')
-- >   (g, v') <- runVarT v x
-- >   pure (h g, t <*> v')) <*> w
--
-- > -- (f >=> g) >=> h = f >=> (g >=> h)
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (h, t)  <- pure ((.) f, pure (.) <*> u')
-- >   (g, v') <- runVarT v x
-- >   pure (h g, t <*> v')) <*> w
--
-- > -- pure x >>= f = f x
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (g, v') <- runVarT v x
-- >   pure ((.) f g, pure (.) <*> u' <*> v')) <*> w
--
-- > -- Definition of <*>
-- > VarT (\x -> do
-- >   (h, t) <-
-- >     runVarT
-- >       (VarT (\y -> do
-- >         (f, u') <- runVarT u y
-- >         (g, v') <- runVarT v y
-- >         pure ((.) f g, pure (.) <*> u' <*> v'))) x
-- >   (a, w') <- runVarT w x
-- >   pure (h a, t <*> w'))
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (h, t) <-
-- >     (\y -> do
-- >       (f, u') <- runVarT u y
-- >       (g, v') <- runVarT v y
-- >       pure ((.) f g, pure (.) <*> u' <*> v')) x
-- >   (a, w') <- runVarT w x
-- >   pure (h a, t <*> w'))
--
-- > -- Application
-- > VarT (\x -> do
-- >   (h, t) <- do
-- >     (f, u') <- runVarT u x
-- >     (g, v') <- runVarT v x
-- >     pure ((.) f g, pure (.) <*> u' <*> v'))
-- >   (a, w') <- runVarT w x
-- >   pure (h a, t <*> w'))
--
-- > -- (f >=> g) >=> h = f >=> (g >=> h)
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (g, v') <- runVarT v x
-- >   (h, t)  <- pure ((.) f g, pure (.) <*> u' <*> v'))
-- >   (a, w') <- runVarT w x
-- >   pure (h a, t <*> w'))
--
-- > -- pure x >>= f = f x
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (g, v') <- runVarT v x
-- >   (a, w') <- runVarT w x
-- >   pure ((.) f g a, pure (.) <*> u' <*> v' <*> w'))
--
-- > -- Definition of .
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (g, v') <- runVarT v x
-- >   (a, w') <- runVarT w x
-- >   pure (f (g a), pure (.) <*> u' <*> v' <*> w'))
--
-- > -- Coinduction
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (g, v') <- runVarT v x
-- >   (a, w') <- runVarT w x
-- >   pure (f (g a), u' <*> (v' <*> w')))
--
-- > -- pure x >>= f = f
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (g, v') <- runVarT v x
-- >   (a, w') <- runVarT w x
-- >   (b, vw) <- pure (g a, v' <*> w')
-- >   pure (f b, u' <*> vw))
--
-- > -- (f >=> g) >=> h = f >=> (g >=> h)
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (b, vw) <- do
-- >     (g, v') <- runVarT v x
-- >     (a, w') <- runVarT w x
-- >     pure (g a, v' <*> w')
-- >   pure (f b, u' <*> vw))
--
-- > -- Abstraction
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (b, vw) <-
-- >     (\y -> do
-- >       (g, v') <- runVarT v y
-- >       (a, w') <- runVarT w y)
-- >       pure (g a, v' <*> w')) x
-- >   pure (f b, u' <*> vw))
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (b, vw) <-
-- >     runVarT
-- >       (VarT (\y -> do
-- >         (g, v') <- runVarT v y
-- >         (a, w') <- runVarT w y)
-- >         pure (g a, v' <*> w')) x
-- >   pure (f b, u' <*> vw))
--
-- > -- Definition of <*>
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (b, vw) <- runVarT (v <*> w) x
-- >   pure (f b, u' <*> vw))
--
-- > -- Definition of <*>
-- > u <*> (v <*> w)
--
--
-- ==Homomorphism
-- > pure f <*> pure a = pure (f a)
--
-- > -- Definition of pure
-- > VarT (\_ -> pure (f, pure f)) <*> pure a
--
-- > -- Definition of pure
-- > VarT (\_ -> pure (f, pure f)) <*> VarT (\_ -> pure (a, pure a))
--
-- > -- Definition of <*>
-- > VarT (\x -> do
-- >   (f', vf') <- runVarT (VarT (\_ -> pure (f, pure f))) x
-- >   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
-- >   pure (f' a', vf' <*> va'))
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (f', vf') <- (\_ -> pure (f, pure f)) x
-- >   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
-- >   pure (f' a', vf' <*> va'))
--
-- > -- Application
-- > VarT (\x -> do
-- >   (f', vf') <- pure (f, pure f)
-- >   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
-- >   pure (f' a', vf' <*> va'))
--
-- > -- pure x >>= f = f x
-- > VarT (\x -> do
-- >   (a', va') <- runVarT (VarT (\_ -> pure (a, pure a))) x
-- >   pure (f a', pure f <*> va'))
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (a', va') <- (\_ -> pure (a, pure a)) x
-- >   pure (f a', pure f <*> va'))
--
-- > -- Application
-- > VarT (\x -> do
-- >   (a', va') <- pure (a, pure a)
-- >   pure (f a', pure f <*> va'))
--
-- > -- pure x >>= f = f x
-- > VarT (\x -> pure (f a, pure f <*> pure a))
--
-- > -- Coinduction
-- > VarT (\x -> pure (f a, pure (f a)))
--
-- > -- Definition of pure
-- > pure (f a)
--
--
-- ==Interchange
-- > u <*> pure y = pure ($ y) <*> u
--
-- > -- Definition of <*>
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (a, y') <- runVarT (pure y) x
-- >   pure (f a, u' <*> y'))
--
-- > -- Definition of pure
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (a, y') <- runVarT (VarT (\_ -> pure (y, pure y))) x
-- >   pure (f a, u' <*> y'))
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (a, y') <- (\_ -> pure (y, pure y)) x
-- >   pure (f a, u' <*> y'))
--
-- > -- Application
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   (a, y') <- pure (y, pure y))
-- >   pure (f a, u' <*> y'))
--
-- > -- pure x >>= f = f
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   pure (f y, u' <*> pure y))
--
-- > -- Coinduction
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   pure (f y, pure ($ y) <*> u'))
--
-- > -- Definition of $
-- > VarT (\x -> do
-- >   (f, u') <- runVarT u x
-- >   pure (($ y) f, pure ($ y) <*> u')
--
-- > -- pure x >>= f = f
-- > VarT (\x -> do
-- >   (g, y') <- pure (($ y), pure ($ y))
-- >   (f, u') <- runVarT u x
-- >   pure (g f, y' <*> u')
--
-- > -- Abstraction
-- > VarT (\x -> do
-- >   (g, y') <- (\_ -> pure (($ y), pure ($ y))) x
-- >   (f, u') <- runVarT u x
-- >   pure (g f, y' <*> u')
--
-- > -- Newtype
-- > VarT (\x -> do
-- >   (g, y') <- runVarT (VarT (\_ -> pure (($ y), pure ($ y)))) x
-- >   (f, u') <- runVarT u x
-- >   pure (g f, y' <*> u')
--
-- > -- Definition of <*>
-- > VarT (\_ -> pure (($ y), pure ($ y))) <*> u
--
-- > -- Definition of pure
-- > pure ($ y) <*> u
