module Main where

import Lib

main :: IO ()
main = play (InWindow "Nice Window" (500,700) (10,10)) white 60 initWorld drawContext handleInput stepGame
