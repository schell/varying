module Main where

import Test.Hspec hiding (after, before)
import Test.QuickCheck
import Control.Varying
import Data.Functor.Identity
import Data.Time.Clock
import Control.Monad.IO.Class

main :: IO ()
main = hspec $ do
  describe "before" $ do
    it "should produce events before a given step" $ do
      let Identity scans = scanVar (1 ~> before 3) $ replicate 4 ()
      scans `shouldBe` [Event 1, Event 2, NoEvent, NoEvent]

  describe "after" $ do
    it "should produce events after a given step" $ do
      let Identity scans = scanVar (1 ~> after 3) $ replicate 4 ()
      scans `shouldBe` [NoEvent, NoEvent, Event 3, Event 4]
  describe "anyE" $ do
    it "should produce on any event" $ do
      let v1 = use 1 (1 ~> before 2)
          v2 = use 2 (1 ~> after 3)
          v3 = always 3
          v = anyE [v1,v2,v3]
          Identity scans = scanVar v $ replicate 4 ()
      scans `shouldBe` [Event 1, Event 3, Event 2, Event 2]
  describe "timeAsPercentageOf" $ do
      it "should run past 1.0" $ do
          let Identity scans = scanVar (timeAsPercentageOf 4)
                                       [1,1,1,1,1 :: Float]
          last scans `shouldSatisfy` (> 1)
      it "should progress by increments of the total" $ do
          let Identity scans = scanVar (timeAsPercentageOf 4)
                                       [1,1,1,1,1 :: Float]
          scans `shouldBe` [0.25,0.5,0.75,1.0,1.25 :: Float]

  describe "tween" $
      it "should step by the dt passed in" $ do
          let Identity scans = scanSpline (tween linear 0 4 (4 :: Float)) 0
                                          [0,1,1,1,1,1]
          scans `shouldBe` [0,1,2,3,4,4]

  describe "untilEvent" $ do
      let Identity scans = scanSpline (3 `untilEvent` (1 ~> after 10)) 0
                                      (replicate 10 ())
      it "should produce output from the value stream until event procs" $
          head scans `shouldBe` 3
      it "should produce output from the value stream until event procs" $
          last scans `shouldBe` 3

  describe "step" $ do
      let s = do step "hey"
                 step ", "
                 step "there"
                 step "."
          Identity scans = scanSpline s "" $ replicate 6 ()
      it "should produce output exactly one time per call" $
        concat scans `shouldBe` "hey, there..."

  describe "fromEvent" $ do
    let s = fromEvent $ var f ~> onJust
        f 0 = Nothing
        f 1 = Just "YES"
        Identity scans = scanSpline s NoEvent [0,0,0,1,0]
    it "should produce NoEvent until it procs" $
      scans `shouldBe` [NoEvent,NoEvent,NoEvent,Event "YES",Event "YES"]

  describe "effect" $ do
    let s :: SplineT () String IO ()
        s = do step "Getting the time..."
               utc <- effect "running" $ getCurrentTime
               let t = head $ words $ show utc
               step t
               step "The End"
    it "should step once, get the time and then step with a string of the time"
       $ do utc <- getCurrentTime
            let t = head $ words $ show utc
            scans <- liftIO $ scanSpline s "" [(), (), ()]
            scans `shouldBe` ["Getting the time...", t, "The End"]
  describe "race" $ do
      let s1 = do step "s10"
                  step "s11"
                  step "s12"
                  return (1 :: Int)
          s2 = do step "s20"
                  step "s21"
                  return True
          r = do step "start"
                 eIntBool <- race (\a b -> concat [a,":",b]) s1 s2
                 case eIntBool of
                   Left i -> step $ "left won with " ++ show i
                   Right b -> step $ "right won with " ++ show b
          Identity scans = scanSpline r "" $ replicate 4 ()
      it "should step twice and left should win" $
        unwords scans `shouldBe` "start s10:s20 s11:s21 right won with True"

  describe "raceMany" $ do
    let s1 = do step "t"
                step "c"
                return 0
        s2 = do step "h"
                step "a"
                return 1
        s3 = do step "e"
                step "t"
                return (2 :: Int)
        s = do x <- raceMany [s1,s2,s3]
               step $ show x
        Identity scans = scanSpline s "" $ replicate 3 ()
    it "should output in parallel (mappend) and return the first or leftmost result" $ unwords scans `shouldBe` "the cat 0"

  describe "capture" $ do
      let r :: Spline () String ()
          r = do x <- capture $ do step "a"
                                   step "b"
                                   return 2
                 case x of
                   (Just "b", 2) -> step "True"
                   _ -> step "False"
          scans = scanSpline r "" $ replicate 3 ()
      it "should end with the last value captured" $
          unwords (concat scans) `shouldBe` "a b True"

  describe "mapOutput" $ do
      let s :: Spline a Char ()
          s = do step 'a'
                 step 'b'
                 step 'c'
                 let f = pure toEnum
                 mapOutput f $ do step 100
                                  step 101
                                  step 102
                 step 'g'
          Identity scans = scanSpline s 'x' $ replicate 7 ()
      it "should map the output" $
          scans `shouldBe` "abcdefg"

  describe "adjustInput" $ do
      let s = var id `untilEvent_` never
          v :: Var a (Char -> Int)
          v = pure fromEnum
          s' = adjustInput v s
          Identity scans = scanSpline s' 0 "abcd"
      it "should" $ scans `shouldBe` [97,98,99,100]
--------------------------------------------------------------------------------
-- Adherance to typeclass laws
--------------------------------------------------------------------------------
  let inc = 1 ~> accumulate (+) 0
      sinc :: Spline a Int (Int, Int)
      sinc = inc `untilEvent` (1 ~> after 3)
      go a = scanSpline a 0 [0..9]
      equal a b = go a `shouldBe` go b

  describe "spline's functor instance" $ do
    let sincf = fmap id sinc
    it "fmap id = id" $ equal sinc sincf
    let g :: (Int, Int) -> (Int, Int)
        g (x,y) = (x + 1, y)
        f (x,y) = (x - 1, y)
        sdot = fmap (g . f) sinc
        sfdot = fmap g $ fmap f sinc
    it "fmap (g . f) = fmap g . fmap f" $ equal sdot sfdot

  describe "spline's applicative instance" $ do
    let ident = pure id <*> sinc
    it "(identity) pure id <*> v = v" $ equal ident sinc
    let pfpx :: Spline a Int Int
        pfpx = pure (+1) <*> pure 1
        pfx = pure (1+1)
    it "(homomorphism) pure f <*> pure x = pure (f x)" $ equal pfpx pfx
    let u :: Spline a Int (Int -> Int)
        u = pure 66 `_untilEvent` (use (+1) $ 1 ~> after 3)
        upy = u <*> pure 1
        pyu = pure ($ 1) <*> u
    it "(interchange) u <*> pure y = pure ($ y) <*> u" $ equal upy pyu
    let v :: Spline a Int (Int -> Int)
        v = pure 66 `_untilEvent` (use (1-) $ 1 ~> after 4)
        w = pure 72 `_untilEvent` (use 3 $ 1 ~> after 1)
        pduvw = pure (.) <*> u <*> v <*> w
        uvw = u <*> (v <*> w)
    it "(compisition) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $
      equal pduvw uvw

  describe "spline's monad instance" $ do
    let h = sinc
        hr = h >>= return
        p :: Spline a Int Int
        p = pure 1

    it "(right identity w/ const) m >>= return == m" $ equal (p >>= return) p
    it "(right identity) m >>= return == m" $ equal h hr
    it "(right identity w/ monadic results) m >>= return == m" $
      (scanVar (runSplineT h 0) [0..9])
        `shouldBe` scanVar (runSplineT hr 0) [0..9]
    let f :: Int -> Spline a String Bool
        f x = do mapM_ (step . show) [0..x]
                 return True
    it "(left identity) return a >>= f == f a" $
      (scanVar (runSplineT (return 3 >>= f) "") [0..9])
        `shouldBe` scanVar (runSplineT (f 3) "") [0..9]
    let m :: Spline a String Int
        m = do step "hey"
               step "dude"
               return 2
        g :: Bool -> Spline a String ()
        g True = do step "okay"
                    step "got it"
        g False = do step "dang"
                     step "missed it"
    it "(associativity) (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $
      (scanVar (runSplineT ((m >>= f) >>= g) "") [0..9])
        `shouldBe` scanVar (runSplineT (m >>= (\x -> f x >>= g)) "") [0..9]
