module Main where

import           Lib
import           Challenges.Set1

main = fiveRands_D


test1 :: IO ()
test1 = do
    name <- getLine
    putStrLn $ "Hello" ++ name
