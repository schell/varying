{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
module Control.Varying.Time where

import Control.Varying.Core
import Control.Concurrent
import Data.Time.Clock

delta :: (Num t, Fractional t, Monad m) => m a -> (a -> a -> t) -> Var m b t
delta m f = Var $ \_ -> do
    t <- m
    return (0, delta' t)
    where delta' t = Var $ \_ -> do
            t' <- m
            let dt = t' `f` t
            return (dt, delta' t')

deltaUTC :: Fractional t => Var IO b t
deltaUTC = delta getCurrentTime (\a b -> realToFrac $ diffUTCTime a b)

delayThread :: Int -> Var IO b b
delayThread t = Var $ \b -> do
    threadDelay t
    return (b, delayThread t)
