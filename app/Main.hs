module Main where

import           Control.Concurrent    (threadDelay)
import           Control.Varying
import           Data.Function         (fix)
import           Data.Functor.Identity (Identity (..))
import           Data.Time.Clock       (diffUTCTime, getCurrentTime)

-- | A simple 2d point type.
data Point = Point { px :: Float
                   , py :: Float
                   } deriving (Show, Eq)


-- | The duration (in seconds) to tween in each direction.
dur :: Float
dur = 3


-- | A novel, start-stop tween.
easeMiddle :: Monad m => Float -> Float -> Float -> TweenT Float Float m ()
easeMiddle start end t = do
  let change = end - start
  tween_ easeOutExpo start              (start + change/2) $ t/2
  tween_ easeInExpo  (start + change/2) end                $ t/2

-- An exponential tween back and forth from 0 to 50 over 1 seconds that
-- loops forever. This spline takes float values of delta time as input,
-- outputs the current x value at every step.
tweenx :: Monad m => TweenT Float Float m ()
tweenx = do
    -- Tween from 0 to 50 over 'dur' seconds
    easeMiddle 0 50 dur
    -- Chain another tween back to the starting position
    easeMiddle 50 0 dur
    -- Loop forever
    tweenx

-- A quadratic tween back and forth from 0 to 50 over 1 seconds that never
-- ends.
tweeny :: Monad m => TweenT Float Float m ()
tweeny = do
    easeMiddle 50 0 dur
    easeMiddle 0 50 dur
    tweeny

-- | Our Point value that varies over time continuously in x and y.
backAndForth :: Monad m => VarT m Float Point
backAndForth =
    -- Turn our splines into continuous output streams. We must provide
    -- a starting value since splines are not guaranteed to be defined at
    -- their edges.
    let x = tweenStream tweenx 0
        y = tweenStream tweeny 0
    in
    -- Construct a varying Point that takes time as an input.
    (Point <$> x <*> y)

main :: IO ()
main = do
  t <- getCurrentTime
  ($ t) . ($ backAndForth) $ fix $ \loop v lastT -> do
    thisT <- getCurrentTime
    -- Here we'll run in the Identity monad using a time delta provided by
    -- getCurrentTime and diffUTCTime.
    let dt = realToFrac $ diffUTCTime thisT lastT
        Identity (Point x y, vNext) = runVarT v dt
        xStr = replicate (round x) ' ' ++ "x" ++ replicate (50 - round x) ' '
        yStr = replicate (round y) ' ' ++ "y" ++ replicate (50 - round y) ' '
        str  = zipWith f xStr yStr
        f 'x' 'y' = '|'
        f 'y' 'x' = '|'
        f a ' '   = a
        f ' ' b   = b
        f _ _     = ' '
    putStrLn str
    threadDelay $ floor $ 1000000 / (20 :: Double)
    loop vNext thisT
