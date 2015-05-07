module Main where

import Varying
import Event
import System.IO
import Data.IORef
import Data.List

main :: IO ()
main = do
    -- Prevent the user's input from showing up until we want it to.
    hSetEcho stdin False

    -- Ensure that we process each character right away, instead of waiting
    -- until the user presses enter.
    hSetBuffering stdin NoBuffering

    mainVar

mainNoVar :: IO ()
mainNoVar = do
    ref <- newIORef []
    let loop = do ch  <- getChar
                  if ch == '\n'
                  then return ()
                  else do str <- readIORef ref
                          let str' = foldBackspace $ ch:str
                          writeIORef ref str'
                          putStrLn $ reverse str'
                          loop
    loop

mainVar :: IO ()
mainVar = whileVar_ (not . ("\n" `isSuffixOf`)) "" $
    varM (const getChar)
      ~> always
      ~> collect
      ~> var foldBackspace
      ~> var reverse
      ~> varM (\s -> putStrLn s >> return s)

foldBackspace :: String -> String
foldBackspace = foldr (\c acc -> dropBackspace $ c:acc) []
    where dropBackspace [] = []
          dropBackspace ss = if 127 == (fromEnum $ head ss)
                               then drop 2 ss
                               else ss
