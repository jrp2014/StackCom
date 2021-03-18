module Main (main) where

import StackCom (projectName)


main :: IO ()
main = putStrLn ("Benchmarks for " ++ projectName)
