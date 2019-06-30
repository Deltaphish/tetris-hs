module Main where

import Lib
import System.Random

main :: IO ()
main = do
    generator <- randomIO
    game (mkStdGen generator)