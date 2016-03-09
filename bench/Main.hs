import Control.Varying
import Criterion.Main
import Debug.Trace

main :: IO ()
main = do
    let v :: Var Int Int
        v = var (+1)
        run v a = fst <$> runVarT v a
    defaultMain $ [ bgroup "runVarT" [ bench "1" $ nf (run $ chain 1) 0
                                     , bench "2" $ nf (run $ chain 2) 0
                                     , bench "4" $ nf (run $ chain 4) 0
                                     , bench "8" $ nf (run $ chain 8) 0
                                     , bench "16" $ nf (run $ chain 16) 0
                                     , bench "32" $ nf (run $ chain 32) 0
                                     , bench "64" $ nf (run $ chain 64) 0
                                     , bench "128" $ nf (run $ chain 128) 0
                                     ]
                  ]
    return ()

chain :: Int -> Var Int Int
chain n = seq x x
  where x = foldl (~>) (var (+1)) $ take (n - 1) $ cycle [var (+1)]
