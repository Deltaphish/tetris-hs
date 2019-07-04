module Main where

import App
import System.Random

main :: IO ()
main = do
    generator <- randomIO
    game (mkStdGen generator)