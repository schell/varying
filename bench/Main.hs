import Control.Varying
import Control.Monad
import Control.Applicative
import Data.Functor.Identity
import Criterion.Main

main :: IO ()
main = do
    let run v a = runIdentity (fst <$> runVarT v a)
    defaultMain [ bgroup "runVarT" [ bench "1" $ nf (run $ chain 1) 0
                                   , bench "2" $ nf (run $ chain 2) 0
                                   , bench "4" $ nf (run $ chain 4) 0
                                   , bench "8" $ nf (run $ chain 8) 0
                                   , bench "16" $ nf (run $ chain 16) 0
                                   , bench "32" $ nf (run $ chain 32) 0
                                   , bench "64" $ nf (run $ chain 64) 0
                                   , bench "128" $ nf (run $ chain 128) 0
                                   ]
                , bgroup "TweenT"
                    [ bench "tweenStream" $
                        nf (run $ tweenStream myTween 0) 0
                    ]
                ]
    return ()

chain :: Int -> Var Int Int
chain n = seq x x
  where x = foldl (>>>) (var (+1)) $ replicate (n - 1) $ var (+1)

myTween :: Tween Float Float ()
myTween = do
  void $ tween_ easeInExpo 0 100 1
  void $ tween_ easeOutExpo 100 0 1
  myTween
