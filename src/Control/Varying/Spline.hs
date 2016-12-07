-- |
--   Module:     Control.Varying.Spline
--   Copyright:  (c) 2015 Schell Scivally
--   License:    MIT
--   Maintainer: Schell Scivally <efsubenovex@gmail.com>
--
--  Using splines we can easily create continuous streams from discontinuous
--  streams. A spline is a monadic layer on top of event streams which are only
--  continuous over a certain domain. The idea is that we use a monad to
--  "run a stream switched by events". This means taking two streams - an output
--  stream and an event stream - combining them into a temporarily producing
--  stream. Once that "stream pair" inhibits (stops producing), the computation
--  completes and returns a result value. That result value is then used to
--  determine the next spline in the sequence.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
module Control.Varying.Spline
  ( -- * Spline
    Spline
    -- * Spline Transformer
  , SplineT(..)
    -- * Creating streams from splines
  , outputStream
    -- * Creating splines from streams
  , untilEvent
  , untilEvent_
  , _untilEvent
  , _untilEvent_
    -- * Other runners
  , scanSpline
    -- * Combinators
  , step
  , race
  , raceAny
  , merge
  , capture
  , mapOutput
  , adjustInput
    -- * Helpers for debugging
  , fromEvent
    -- * Hand Proofs of the Monad laws
    -- $proofs
  ) where

import Control.Varying.Core
import Control.Varying.Event
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Monoid

-- stuff for FAMP
#if __GLASGOW_HASKELL__ <= 707
import Control.Applicative
import Data.Function
#endif

-- $setup
-- >>> import Control.Varying.Time

-- | 'SplineT' shares all the types of 'VarT' and adds a result value. Its
-- monad, input and output types (@m@, @a@ and @b@, respectively) represent the
-- same parameters in 'VarT'. A spline adds a result type which represents the
-- monadic computation's result value.
--
-- A spline either concludes in a result or it produces an output value and
-- another spline. This makes it a stream that eventually ends. We can use this
-- to set up our streams in a monadic fashion, where the end result of one spline
-- can be used to determin the next spline to run. Using 'outputStream' we can
-- then fuse these piecewise continuous (but otherwise discontinuous) streams
-- into one continuous stream of type @VarT m a b@.
newtype SplineT a b m c =
  SplineT { runSplineT :: a -> m (Either c (b, SplineT a b m c)) }

-- | A spline is a functor by applying the function to the result of the
-- spline. This does just what you would expect of other Monads such as 'StateT'
-- or 'Maybe'.
--
-- >>> :{
-- let s0 = pure "first" `untilEvent` (1 >>> after 2)
--     s = do str <- fmap show s0
--            step str  
--     v = outputStream s "" 
-- in testVarOver v [(),()]
-- >>> :}
-- "first"
-- "(\"first\",2)"
instance (Applicative m, Monad m) => Functor (SplineT a b m) where
  fmap f (SplineT s) = SplineT $ s >=> \case
    Left c        -> return $ Left $ f c
    Right (b, s1) -> return $ Right (b, fmap f s1)

-- | A spline responds to bind by running until it concludes in a value,
-- then uses that value to run the next spline.
--
-- Note - checkout the <$proofs proofs>
instance (Applicative m, Monad m) => Monad (SplineT a b m) where
  return = SplineT . const . return . Left
  (SplineT s0) >>= f = SplineT $ g s0
    where g s a = do e <- s a
                     case e of
                       Left  c               -> runSplineT (f c) a
                       Right (b, SplineT s1) -> return $ Right (b, SplineT $ g s1)

-- A spline responds to 'pure' by returning a spline that never produces an
-- output value and immediately returns the argument. It responds to '<*>' by
-- applying the left arguments result value (the function) to the right
-- arguments result value (the argument), sequencing them both in serial.
--
-- @
-- pure = return
-- sf <*> sx = do
--   f <- sf
--   x <- sx
--   return $ f x
-- @
instance (Applicative m, Monad m) => Applicative (SplineT a b m) where
  pure = return
  sf <*> sx = do
    f <- sf
    x <- sx
    return $ f x

-- | A spline is a transformer by running the effect and immediately concluding,
-- using the effect's result as the result value. 
-- 
-- >>> :{
-- let s = do () <- lift $ print "Hello"  
--            step 2
--     v = outputStream s 0
-- in testVarOver v [()]
-- >>> :}
-- "Hello"
-- 2
instance MonadTrans (SplineT a b) where
  lift f = SplineT $ const $ fmap Left f

-- | A spline can do IO if its underlying monad has a MonadIO instance. It
-- takes the result of the IO action as its immediate return value.
instance (Applicative m, Monad m, MonadIO m) => MonadIO (SplineT a b m) where
  liftIO = lift . liftIO

-- | A SplineT monad parameterized with Identity that takes input of type @a@,
-- output of type @b@ and a result value of type @c@.
type Spline a b c = SplineT a b Identity c

-- | Permute a spline into one continuous stream. Since a spline is not
-- guaranteed to be defined over any domain (specifically on its edges), this
-- function takes a default value to use as the "last known value".
--
-- >>> :{
-- let s :: SplineT () String IO ()  
--     s = do first <- pure "accumulating until 3" `_untilEvent` (1 >>> after 3)
--            secnd <- pure "accumulating until 4" `_untilEvent` (1 >>> after 4)
--            if first + secnd == 7
--              then step "done"
--              else step "something went wrong!"
--     v = outputStream s ""
-- in testVarOver v $ replicate 6 ()
-- >>> :}
-- "accumulating until 3"
-- "accumulating until 3"
-- "accumulating until 4"
-- "accumulating until 4"
-- "accumulating until 4"
-- "done"
outputStream :: (Applicative m, Monad m)
             => SplineT a b m c -> b -> VarT m a b
outputStream (SplineT s0) b0 = VarT $ f s0 b0
  where f s b a = do e <- s a
                     case e of
                       Left  _                -> return (b, done b)
                       Right (b1, SplineT s1) -> return (b1, VarT $ f s1 b1)

-- | Run the spline over the input values, gathering the output values in a
-- list.
scanSpline :: (Applicative m, Monad m)
           => SplineT a b m c -> b -> [a] -> m [b]
scanSpline s b = fmap fst <$> scanVar (outputStream s b)

-- | Create a spline from an event stream.
fromEvent :: (Applicative m, Monad m) => VarT m a (Event b) -> SplineT a (Event b) m b
fromEvent ve = SplineT $ \a -> do
  (e, ve1) <- runVarT ve a
  return $ case e of
    Just b -> Left b
    Nothing -> Right (Nothing, fromEvent ve1)

-- | Create a spline from a stream and an event stream. The spline
-- uses the stream as its output value. The spline will run until
-- the event stream produces an event, at that point the last known output
-- value and the event value are tupled and returned as the spline's result.
untilEvent :: (Applicative m, Monad m)
           => VarT m a b -> VarT m a (Event c) -> SplineT a b m (b,c)
untilEvent v ve = SplineT $ f ((,) <$> v <*> ve)
  where f vve a = do t <-runVarT vve a
                     return $ case t of
                       ((b, Nothing), vve1) -> Right (b, SplineT $ f vve1)
                       ((b, Just c),    _) -> Left (b, c)

-- | A variant of 'untilEvent' that results in the last known output value.
untilEvent_ :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c) -> SplineT a b m b
untilEvent_ v ve = fst <$> untilEvent v ve

-- | A variant of 'untilEvent' that results in the event steam's event value.
_untilEvent :: (Applicative m, Monad m)
            => VarT m a b -> VarT m a (Event c) -> SplineT a b m c
_untilEvent v ve = snd <$> untilEvent v ve

-- | A variant of 'untilEvent' that discards both the output and event values.
_untilEvent_ :: (Applicative m, Monad m)
             => VarT m a b -> VarT m a (Event c) -> SplineT a b m ()
_untilEvent_ v ve = void $ _untilEvent v ve

-- | Run two splines in parallel, combining their output. Return the result of
-- the spline that concludes first. If they conclude at the same time the result
-- is taken from the left spline.
--
-- >>> :{
-- let s1 = pure "route "   `_untilEvent` (1 >>> after 2)
--     s2 = pure 666     `_untilEvent` (1 >>> after 3)
--     s = do winner <- race (\l r -> l ++ show r) s1 s2
--            step $ show winner
--     v = outputStream s ""
-- in testVarOver v [(),(),()]
-- >>> :}
-- "route 666"
-- "Left 2"
-- "Left 2"
race :: (Applicative m, Monad m)
     => (a -> b -> c) -> SplineT i a m d -> SplineT i b m e
     -> SplineT i c m (Either d e)
race f sa0 sb0 = SplineT (g sa0 sb0)
  where g sa sb i = runSplineT sa i >>= \case
          Left d -> return $ Left $ Left d
          Right (a, sa1) -> runSplineT sb i >>= \case
            Left e -> return $ Left $ Right e
            Right (b, sb1) -> return $ Right (f a b, SplineT $ g sa1 sb1)

-- | Run many splines in parallel, combining their output with 'mappend'.
-- Returns the result of the spline that concludes first. If any conclude at the
-- same time the leftmost result will be returned.
--
-- >>> :{
-- let ss = [ pure "hey "   `_untilEvent` (1 >>> after 5)
--          , pure "there"  `_untilEvent` (1 >>> after 3)
--          , pure "!"      `_untilEvent` (1 >>> after 2) 
--          ]
--     s = do winner <- raceAny ss
--            step $ show winner
--     v = outputStream s ""
-- in testVarOver v [(),()]
-- >>> :}
-- "hey there!"
-- "2"
raceAny :: (Applicative m, Monad m, Monoid b)
         => [SplineT a b m c] -> SplineT a b m c
raceAny [] = pure mempty `_untilEvent` never
raceAny ss = SplineT $ f [] (map runSplineT ss) mempty
  where f ys []     b _    = return $ Right (b, SplineT $ f [] ys mempty)
        f ys (v:vs) b a = v a >>= \case
          Left c -> return $ Left c
          Right (b1, s) -> f (ys ++ [runSplineT s]) vs (b <> b1) a

-- | Run two splines in parallel, combining their output. Once both splines
-- have concluded, return the results of each in a tuple.
--
-- >>> :{
-- let s1 = pure "hey "   `_untilEvent` (1 >>> after 3)
--     s2 = pure "there!" `_untilEvent` (1 >>> after 2)
--     s  = do tuple <- merge (++) s1 s2
--             step $ show tuple
--     v  = outputStream s ""
-- in testVarOver v [(),(),()]
-- >>> :}
-- "hey there!"
-- "hey "
-- "(3,2)" 
merge :: (Applicative m, Monad m)
     => (b -> b -> b)
     -> SplineT a b m c -> SplineT a b m d -> SplineT a b m (c, d)
merge apnd s1 s2 = SplineT $ f s1 s2

  where r c d = return $ Left (c, d)

        fr c vb a = runSplineT vb a >>= \case
          Left d -> r c d
          Right (b, vb1) -> return $ Right (b, SplineT $ fr c vb1)

        fl d va a = runSplineT va a >>= \case
          Left c -> r c d
          Right (b, va1) -> return $ Right (b, SplineT $ fl d va1)

        f va vb a = runSplineT va a >>= \case
          Left c -> fr c vb a
          Right (b1, va1) -> runSplineT vb a >>= \case
            Left d -> return $ Right (b1, SplineT $ fl d va1)
            Right (b2, vb1) -> return $ Right (apnd b1 b2, SplineT $ f va1 vb1)

-- | Capture the spline's last output value and tuple it with the
-- spline's result. This is helpful when you want to sample the last
-- output value in order to determine the next spline to sequence.
capture :: (Applicative m, Monad m)
        => SplineT a b m c -> SplineT a b m (Event b, c)
capture = SplineT . f Nothing
    where f mb s a = runSplineT s a >>= \case
            Left c -> return $ Left (mb, c)
            Right (b, s1) -> return $ Right (b, SplineT $ f (Just b) s1)

-- | Produce the argument as an output value exactly once.
--
-- >>> :{
-- let s = do step "hi"
--            step "there"
--            step "friend"
-- in testVarOver (outputStream s "") [1,2,3,4]
-- >>> :}
-- "hi"
-- "there"
-- "friend"
-- "friend"
step :: (Applicative m, Monad m) => b -> SplineT a b m ()
step b = SplineT $ const $ return $ Right (b, return ())

-- | Map the output value of a spline.
--
-- >>> :{
-- let s = mapOutput (pure show) $ step 1 >> step 2 >> step 3  
-- in testVarOver (outputStream s "") [(),(),()]    
-- >>> :}
-- "1"
-- "2"
-- "3"
mapOutput :: (Applicative m, Monad m)
          => VarT m a (b -> t) -> SplineT a b m c -> SplineT a t m c
mapOutput vf0 s0 = SplineT $ g vf0 s0
    where g vf s a = do
            (f, vf1) <- runVarT vf a
            runSplineT s a >>= \case
              Left c -> return $ Left c
              Right (b, s1) -> return $ Right (f b, SplineT $ g vf1 s1)

-- | Map the input value of a spline.
adjustInput :: (Applicative m, Monad m)
            => VarT m a (a -> r) -> SplineT r b m c -> SplineT a b m c
adjustInput vf0 s = SplineT $ g vf0 s
  where g vf sx (!a) = do
          (f, vf1) <- runVarT vf a
          runSplineT sx (f a) >>= \case
           Left c -> return $ Left c
           Right (b, sx1) -> return $ Right (b, SplineT $ g vf1 sx1)

--------------------------------------------------------------------------------
-- $proofs 
-- ==Left Identity
-- > k =<< return c = k c
--
-- > -- Definition of =<<
-- > fix (\f s ->
-- >   SplineT (\a ->
-- >     runSplineT s a >>= \case
-- >       Left c -> runSplineT (k c) a
-- >       Right s' -> return (Right (fmap f s')))) (return c)
--
-- > -- Definition of fix
-- > (\s ->
-- >   SplineT (\a ->
-- >     runSplineT s a >>= \case
-- >       Left c -> runSplineT (k c) a
-- >       Right s' -> return (Right (fmap (k =<<) s')))) (return c)
--
-- > -- Application
-- > SplineT (\a ->
-- >   runSplineT (return c) a >>= \case
-- >     Left c -> runSplineT (k c) a
-- >     Right s' -> return (Right (fmap (k =<<) s')))
--
-- > -- Definition of return
-- > SplineT (\a ->
-- >   runSplineT (SplineT (\_ -> return (Left c))) a >>= \case
-- >     Left c -> runSplineT (k c) a
-- >     Right s' -> return (Right (fmap (k =<<) s')))
--
-- > -- Newtype
-- > SplineT (\a ->
-- >   (\_ -> return (Left c)) a >>= \case
-- >     Left c -> runSplineT (k c) a
-- >     Right s' -> return (Right (fmap (k =<<) s')))
--
-- > -- Application
-- > SplineT (\a ->
-- >   return (Left c) >>= \case
-- >     Left c -> runSplineT (k c) a
-- >     Right s' -> return (Right (fmap (k =<<) s')))
--
-- > -- return x >>= f = f x
-- > SplineT (\a ->
-- >   case (Left c) of
-- >     Left c -> runSplineT (k c) a
-- >     Right s' -> return (Right (fmap (k =<<) s')))
--
-- > -- Case evaluation
-- > SplineT (\a -> runSplineT (k c) a)
--
-- > -- Eta reduction
-- > SplineT (runSplineT (k c))
--
-- > -- Newtype
-- > k c
--
-- ==Right Identity
-- > return =<< m = m
--
-- > -- Definition of =<<
-- > fix (\f s ->
-- >   SplineT (\a ->
-- >     runSplineT s a >>= \case
-- >       Left c -> runSplineT (return c) a
-- >       Right s' -> return (Right (fmap f s')))) m
--
-- > -- Definition of fix
-- > (\s ->
-- >   SplineT (\a ->
-- >     runSplineT s a >>= \case
-- >       Left c -> runSplineT (return c) a
-- >       Right s' -> return (Right (fmap (return =<<) s')))) m
--
-- > -- Application
-- > SplineT (\a ->
-- >   runSplineT m a >>= \case
-- >     Left c -> runSplineT (return c) a
-- >     Right s' -> return (Right (fmap (return =<<) s')))
--
-- > -- Definition of return
-- > SplineT (\a ->
-- >   runSplineT m a >>= \case
-- >     Left c -> runSplineT (SplineT (\_ -> return (Left c))) a
-- >     Right s' -> return (Right (fmap (return =<<) s')))
--
-- > -- Newtype
-- > SplineT (\a ->
-- >   runSplineT m a >>= \case
-- >     Left c -> (\_ -> return (Left c)) a
-- >     Right s' -> return (Right (fmap (return =<<) s')))
--
-- > -- Application
-- > SplineT (\a ->
-- >   runSplineT m a >>= \case
-- >     Left c -> return (Left c)
-- >     Right s' -> return (Right (fmap (return =<<) s')))
--
-- > -- m >>= return . f = fmap f m
-- > SplineT (\a -> fmap (either id (fmap (return =<<))) (runSplineT m a))
--
-- > -- Coinduction
-- > SplineT (\a -> fmap (either id (fmap id)) (runSplineT m a))
--
-- > -- fmap id = id
-- > SplineT (\a -> fmap (either id id) (runSplineT m a))
--
-- > -- either id id = id
-- > SplineT (\a -> fmap id (runSplineT m a))
--
-- > -- fmap id = id
-- > SplineT (\a -> runSplineT m a)
--
-- > -- Eta reduction
-- > SplineT (runSplineT m)
--
-- > -- Newtype
-- > m
--
-- ==Application
-- > (m >>= f) >>= g = m >>= (\x -> f x >>= g)

-- TODO
