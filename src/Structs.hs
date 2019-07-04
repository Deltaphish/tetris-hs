module Structs(
  Context(Context)
, shape
, next
, posX
, posY
, velX
, velY
, rot
, field
, score
, time
, generator
, chooseShapeList
, Shape(
    Square
  , Lblock
  , RLblock
  , Squig
  , RSquig
  , Tblock
  , Straight)
) where

import qualified System.Random

data Context = Context {
  shape :: Shape
, next  :: Shape
, posX  :: Int
, posY  :: Int
, velX  :: Int
, velY  :: Int
, rot   :: Int
, field :: [[Bool]]
, score :: Int
, time  :: Float
, generator  :: System.Random.StdGen
}

data Shape = Square [(Int,Int)] 
           | Lblock [(Int,Int)] 
           | RLblock [(Int,Int)] 
           | Squig [(Int,Int)] 
           | RSquig [(Int,Int)] 
           | Tblock [(Int,Int)]
           | Straight [(Int,Int)]

square = Square [(0,0),(1,0),(1,-1),(0,-1)]
lblock = Lblock [(0,0),(0,1),(0,-1),(1,-1)]
rblock = RLblock [(0,0),(0,1),(0,-1),(-1,-1)]
squig = Squig [(0,0),(1,0),(-1,-1),(0,-1)]
rsquig = RSquig [(0,0),(-1,0),(1,-1),(0,-1)]
straight = Straight [(0,0),(-1,0),(1,0),(2,0)]
tblock = Tblock [(0,0),(1,0),(-1,0),(0,1)]

chooseShapeList = [square,lblock,rblock,square,rsquig,straight,tblock]
