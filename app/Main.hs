module Main where

import Control.Varying
import Control.Applicative
import Control.Concurrent (forkIO, killThread)
import Data.Functor.Identity
import Data.Time.Clock

-- | A simple 2d point type.
data Point = Point { px :: Float
                   , py :: Float
                   } deriving (Show, Eq)

newtype Delta = Delta { unDelta :: Float }

-- An exponential tween back and forth from 0 to 100 over 2 seconds that
-- loops forever. This spline takes float values of delta time as input,
-- outputs the current x value at every step and would result in () if it
-- terminated.
tweenx :: (Applicative m, Monad m) => SplineT Float Float m Float
tweenx = do
    -- Tween from 0 to 100 over 1 second
    x <- tween easeOutExpo 0 50 1
    -- Chain another tween back to the starting position
    _ <- tween easeOutExpo x 0 1
    -- Loop forever
    tweenx

-- A quadratic tween back and forth from 0 to 100 over 2 seconds that never
-- ends.
tweeny :: (Applicative m, Monad m) => SplineT Float Float m Float
tweeny = do
    y <- tween easeOutExpo 50 0 1
    _ <- tween easeOutExpo y 50 1
    tweeny

-- Our time signal counts input delta time samples.
time :: Monad m => VarT m Delta Float
time = var unDelta

-- | Our Point value that varies over time continuously in x and y.
backAndForth :: Monad m => VarT m Delta Point
backAndForth =
    -- Turn our splines into continuous output streams. We must provide
    -- a starting value since splines are not guaranteed to be defined at
    -- their edges.
    let x = outputStream tweenx 0
        y = outputStream tweeny 0
    in
    -- Construct a varying Point that takes time as an input.
    (Point <$> x <*> y)
        -- Stream in a time signal using the 'plug left' combinator.
        -- We could similarly use the 'plug right' (~>) function
        -- and put the time signal before the construction above. This is needed
        -- because the tween streams take time as an input.
        <~ time

main :: IO ()
main = do
    putStrLn "An example of value streams using the varying library."
    putStrLn "Enter a newline to continue, and then a newline to quit"
    _ <- getLine

    t   <- getCurrentTime
    tId <- forkIO $ loop backAndForth t

    _ <- getLine
    killThread tId

loop :: Var Delta Point -> UTCTime -> IO ()
loop v t = do
  t1 <- getCurrentTime
  -- Here we'll run in the Identity monad using a fixed time step.
  let dt = realToFrac $ diffUTCTime t1 t
      Identity (Point x y, vNext) = runVarT v $ Delta dt
      xStr = replicate (round x) ' ' ++ "x" ++ replicate (50 - round x) ' '
      yStr = replicate (round y) ' ' ++ "y" ++ replicate (50 - round y) ' '
      str  = zipWith f xStr yStr
      f 'x' 'y' = '|'
      f 'y' 'x' = '|'
      f a ' ' = a
      f ' ' b = b
      f _ _ = ' '
  putStrLn str
  --threadDelay 10
  loop vNext t1

