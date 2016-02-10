module Main where

import Test.Hspec hiding (after)
import Test.QuickCheck
import Control.Varying
import Data.Functor.Identity

main :: IO ()
main = hspec $ do 
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
            let Identity scans = scanSpline (tween linear 0 4 (4 :: Float)) 
                                            [0,1,1,1,1,1] 
            scans `shouldBe` [(Event 0, NoEvent)
                             ,(Event 1, NoEvent)
                             ,(Event 2, NoEvent)
                             ,(Event 3, NoEvent)
                             ,(Event 4, NoEvent)
                             ,(Event 4, Event 4)
                             ]

    describe "untilEvent" $ do
        let Identity scans = scanSpline (3 `untilEvent` (1 ~> after 10))
                                        (replicate 10 ())
        it "should produce output from the value stream until event procs" $
            head scans `shouldBe` (Event 3, NoEvent)
        it "should produce output from the value stream until event procs" $
            last scans `shouldBe` (Event 3, Event (3,()))

    describe "pair" $ do
        let s1 = 3 `untilEvent_` (1 ~> after 10)
            s2 = do 4 `untilEvent_` (1 ~> after 10)
                    5 `untilEvent_` (1 ~> after 10)
            Identity scans = scanSpline (pair (+) s1 s2) $ replicate 20 () 
        it "should end" $
            length (takeWhile ((== NoEvent) . snd) scans) `shouldBe` 18 
        it "should combine output" $
            head scans `shouldBe` (Event 7, NoEvent)
        it "should progress" $
            (scans !! 11) `shouldBe` (Event 8, NoEvent)
        it "should pair both results" $
            last scans `shouldBe` (Event 8, Event (3,5))

    describe "race" $ do
        let s1 = pure 'a' `untilEvent_` (1 ~> after 3)
            s2 = pure 'x' `untilEvent_` (1 ~> after 4)
            r  = race (\a x -> [a,x]) s1 s2
            Identity scans = scanSpline r $ replicate 20 ()
        it "should combine output" $
            head scans `shouldBe` (Event "ax", NoEvent) 
        it "should end" $
            length (takeWhile ((== NoEvent) . snd) scans) `shouldBe` 2
        it "should show 'a' as winner" $
            last scans `shouldBe` (Event "ax", Event 'a')

    describe "capture" $ do
        let fstr str char = str ++ [char]
            s = (1 ~> accumulate (+) (fromEnum 'a') 
                   ~> var toEnum 
                   ~> accumulate fstr "") 
                   `untilEvent_` (1 ~> after 3)
            Identity scans = scanSpline (capture s) $ replicate 5 ()
        it "should end with the last value captured" $ 
            scans !! 2 `shouldBe` (Event "bcd", Event (Just "bcd", "bcd")) 
    
    describe "step" $ do
        let s = step "hey"
            Identity scans = scanSpline s $ replicate 3 ()
        it "should produce exactly once" $ do
            head scans `shouldBe` (Event "hey", NoEvent)
            scans !! 1 `shouldBe` (Event "hey", Event ())

    describe "mapOutput" $ do
        let s :: Spline () String String 
            s = pure "hey" `untilEvent_` never
            f :: Int -> Char -> Int
            f acc char = acc + fromEnum char
            g :: String -> Int
            g = foldl f 0
            v :: Var () (String -> Int)
            v = var $ const g 
            s' = mapOutput v s 
            Identity scans = scanSpline s' $ replicate 3 ()
        it "should map the output" $ 
            head scans `shouldBe` (Event 326, NoEvent) 

    describe "adjustInput" $ do
        let s = var id `untilEvent_` never
            v :: Var a (Char -> Int) 
            v = pure fromEnum 
            s' = adjustInput v s
            Identity scans = scanSpline s' "abcd"
        it "should" $ map fst scans `shouldBe` [ Event 97
                                               , Event 98
                                               , Event 99
                                               , Event 100
                                               ]
