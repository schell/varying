module Main where

import Control.Varying
import Control.Applicative
import Control.Monad (void)
import Data.Functor.Identity
import Data.Function (fix)
import Data.Time.Clock

-- | A simple 2d point type.
data Point = Point { px :: Float
                   , py :: Float
                   } deriving (Show, Eq)

-- An exponential tween back and forth from 0 to 50 over 1 seconds that
-- loops forever. This spline takes float values of delta time as input,
-- outputs the current x value at every step.
tweenx :: (Applicative m, Monad m) => TweenT Float Float m Float
tweenx = do
    -- Tween from 0 to 50 over 1 second
    tween_ easeOutExpo 0 50 1
    -- Chain another tween back to the starting position
    tween_ easeOutExpo 50 0 1
    -- Loop forever
    tweenx

-- A quadratic tween back and forth from 0 to 50 over 1 seconds that never
-- ends.
tweeny :: (Applicative m, Monad m) => TweenT Float Float m Float
tweeny = do
    tween_ easeOutExpo 50 0 1
    tween_ easeOutExpo 0 50 1
    tweeny

-- | Our Point value that varies over time continuously in x and y.
backAndForth :: (Applicative m, Monad m) => VarT m Float Point
backAndForth =
    -- Turn our splines into continuous output streams. We must provide
    -- a starting value since splines are not guaranteed to be defined at
    -- their edges.
    let x = tweenStream tweenx 0
        y = tweenStream tweeny 0
    in
    -- Construct a varying Point that takes time as an input.
    (Point <$> x <*> y)

-- | An example of using 'fix' and combining splines to get a better programming
-- experience while writing tweens.
betterBackAndForth :: (Applicative m, Monad m) => VarT m Float Point
betterBackAndForth = flip tweenStream (Point 0 0) $ fix $ \nxt -> do
  void $ race Point (tween_ easeOutExpo 0 50 1) (tween_ easeOutExpo 50 0 1)
  void $ race Point (tween_ easeOutExpo 50 0 1) (tween_ easeOutExpo 0 50 1)
  nxt

main :: IO ()
main = getCurrentTime >>= loop betterBackAndForth

loop :: Var Float Point -> UTCTime -> IO ()
loop v t = do
  t1 <- getCurrentTime
  -- Here we'll run in the Identity monad using a time delta provided by
  -- getCurrentTime and diffUTCTime.
  let dt = realToFrac $ diffUTCTime t1 t
      Identity (Point x y, vNext) = runVarT v dt
      xStr = replicate (round x) ' ' ++ "x" ++ replicate (50 - round x) ' '
      yStr = replicate (round y) ' ' ++ "y" ++ replicate (50 - round y) ' '
      str  = zipWith f xStr yStr
      f 'x' 'y' = '|'
      f 'y' 'x' = '|'
      f a ' ' = a
      f ' ' b = b
      f _ _ = ' '
  putStrLn str
  loop vNext t1
