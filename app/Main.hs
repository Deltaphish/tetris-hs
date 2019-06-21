module Main where

import Lib

main :: IO ()
main = play (InWindow "Nice Window" (500,700) (10,10)) white 1 initWorld drawContext handleInput stepGame
