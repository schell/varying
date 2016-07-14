import Control.Varying
import Control.Monad
import Control.Applicative
import Data.Functor.Identity
import Criterion.Main

main :: IO ()
main = do
    let run v a = runIdentity (fst <$> runVarT v a)
    defaultMain $ [ bgroup "runVarT" [ bench "1" $ nf (run $ chain 1) 0
                                     , bench "2" $ nf (run $ chain 2) 0
                                     , bench "4" $ nf (run $ chain 4) 0
                                     , bench "8" $ nf (run $ chain 8) 0
                                     , bench "16" $ nf (run $ chain 16) 0
                                     , bench "32" $ nf (run $ chain 32) 0
                                     , bench "64" $ nf (run $ chain 64) 0
                                     , bench "128" $ nf (run $ chain 128) 0
                                     ]
                  , bgroup "SplineT"
                      [ bench "runSplineT" $
                          nf (run $ outputStream spline 0) 0
                      ]
                  ]
    return ()

chain :: Int -> Var Int Int
chain n = seq x x
  where x = foldl (~>) (var (+1)) $ take (n - 1) $ cycle [var (+1)]

spline :: Spline Float Float ()
spline = do
  void $ tween easeInExpo 0 100 1
  void $ tween easeOutExpo 100 0 1
  spline
