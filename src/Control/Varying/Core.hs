module Control.Varying.Core where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Applicative

-- | A kind of Mealy machine with effects.
data Var m b c = Var { runVar :: b -> m (c, Var m b c) }

var :: Applicative a => (b -> c) -> Var a b c
var f = Var $ \a -> pure $ (f a, var f)

varM :: Monad m => (a -> m b) -> Var m a b
varM f = Var $ \a -> do
    b <- f a
    return (b, varM f)

evalVar :: Functor m => Var m a b -> a -> m b
evalVar v a = fst <$> (runVar v a)

execVar :: Functor m => Var m a b -> a -> m (Var m a b)
execVar v a = snd <$> (runVar v a)

loopVar :: Monad m => a -> Var m a a -> m a
loopVar a v = runVar v a >>= uncurry loopVar

loopVar_ :: Monad m => Var m () a -> m ()
loopVar_ v = execVar v () >>= loopVar_

whileVar :: Monad m => (a -> Bool) -> a -> Var m a a -> m a
whileVar f a v = if f a
                 then runVar v a >>= uncurry (whileVar f)
                 else return a

whileVar_ :: Monad m => (a -> Bool) -> a -> Var m a a -> m ()
whileVar_ f a v = whileVar f a v >> return ()

testVar :: (Read a, Show b) => Var IO a b -> IO ()
testVar v = loopVar_ $ varM (const $ putStrLn "input: ")
                    ~> varM (const getLine)
                    ~> var read
                    ~> v
                    ~> varM (putStrLn . show)

-- | Plugs the output value of v1 into the input value of v2.
(~>) :: Monad m => Var m a b -> Var m b c -> Var m a c
(~>) v1 v2 = Var $ \a -> do
    (b, v1') <- runVar v1 a
    (c, v2') <- runVar v2 b
    return $ (c, v1' ~> v2')
infixr 1 ~>

(<~) :: Monad m => Var m b c -> Var m a b -> Var m a c
(<~) = flip (~>)
infixl 1 <~

-- | Folds input values into an accumulator using a fold function and yields
-- that accumulator each step.
foldWith :: Monad m => (c -> b -> c) -> c -> Var m b c
foldWith f b = Var $ \a -> do
    let b' = f b a
    return (b', foldWith f b')

delay :: Monad m => b -> Var m a b -> Var m a b
delay b v = Var $ \a -> do
    (b', v') <- runVar v a
    return (b, delay b' v')

instance Monad m => Functor (Var m b) where
    fmap f' v = v ~> var f'

instance Monad m => Category (Var m) where
    id = var id
    (.) = (<~)

instance Monad m => Applicative (Var m a) where
    pure = var . const
    vf <*> va = Var $ \a -> do (f, vf') <- runVar vf a
                               (b, va') <- runVar va a
                               return $ (f b, vf' <*> va')

instance Monad m => Arrow (Var m) where
    arr = var
    first v = Var $ \(b,d) -> do (c, v') <- runVar v b
                                 return $ ((c,d), first v')

instance (Monad m, Num b) => Num (Var m a b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

instance (Monad m, Floating b) => Floating (Var m a b) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin; sinh = fmap sinh; asin = fmap asin; asinh = fmap asinh
    cos = fmap cos; cosh = fmap cosh; acos = fmap acos; acosh = fmap acosh
    atan = fmap atan; atanh = fmap atanh

instance (Monad m, Fractional b) => Fractional (Var m a b) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational
