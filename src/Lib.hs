module Lib
    (
    initWorld
   ,drawContext
   ,handleInput
   ,stepGame
   ,play
   ,Display(InWindow)
   ,white
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List

data Shape = Square [(Int,Int)] 
           | Lblock [(Int,Int)] 
           | RLblock [(Int,Int)] 
           | Squig [(Int,Int)] 
           | RSquig [(Int,Int)] 
           | Straight [(Int,Int)] deriving (Show)

getCoords :: Shape -> [(Int,Int)]
getCoords (Square arr) = arr

data Context = Context {
  shape :: Shape
, next  :: Shape
, posX  :: Int
, posY  :: Int
, velX  :: Int
, velY  :: Int
, rot   :: Int
, field :: [Bool]
, score :: Int
} deriving (Show)

projectNext :: Context -> Context
projectNext ctx = ctx {next = Square $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) (getCoords (shape ctx))}

checkField :: Int -> Int -> Context -> Bool
checkField col row ctx = field ctx !! (row*10 + col)

setField :: Int -> Int -> Context -> Context
setField 0 0 ctx = ctx {field = newField} where newField = True : tail (field ctx)
setField col row ctx = ctx {field = init a ++ True : b} where (a,b) = splitAt (row*10+col+1) (field ctx)

colPos :: (Int,Int) -> Context -> Bool
colPos (_,-1) _ = True
colPos (col,row) ctx = field ctx !! (col + row*10)

collider :: Context -> Bool
collider ctx = True `elem` [colPos (a + posX ctx,b + posY ctx) ctx | (a,b) <- getCoords (next ctx)]


placeShape :: Context -> Context
placeShape ctx = foldr (\(a,b) ct -> setField (a + posX ctx) (b + posY ctx) ct) ctx $ getCoords $ shape ctx

spawnNew :: Context -> Context
spawnNew ctx = ctx { posY = 20, posX = 5}

moveTermino :: Context -> Context
moveTermino ctx 
  | not(collider $ projectNext ctx) = ctx { posY = posY ctx + velY ctx, posX = posX ctx + velX ctx, velX = 0, velY = -1}
  | otherwise = spawnNew $ placeShape ctx

standardBlock = rectangleSolid 20 20
standardBorder = rectangleWire 22 22

greyBlock = Pictures [Color (greyN 0.6) standardBlock,Color (dark yellow) standardBorder]
yellowBlock = Pictures [Color yellow standardBlock,Color (dark yellow) standardBorder]
redBlock = Pictures [Color red standardBlock,Color (dark red) standardBorder]

createPath :: Int -> Int -> Path
createPath col row = map (\(a,b) -> (a+ fromIntegral col,b + fromIntegral row)) bx where bx = [(-10,-10),(-10,10),(10,10),(10,-10)]

drawGrid :: Context -> [Picture]
drawGrid ctx = [Translate (fromIntegral col*22.0) (fromIntegral row*22.0 - 22.0*7.0) greyBlock | a <- elemIndices True $ field ctx,let col = a `mod` 10,let row = a `quot` 10]

drawShape :: Context -> [Picture]
drawShape ctx =  [Translate (fromIntegral (col+posX ctx)*22.0) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0) yellowBlock | let a = getCoords $ shape ctx,(col,row) <- a]

drawContext :: Context -> Picture
drawContext ctx = Pictures $ drawShape ctx ++ drawGrid ctx

stepGame :: Float -> Context -> Context
stepGame fl = moveTermino

handleInput :: Event -> Context -> Context
handleInput (EventKey key _ _ _) ctx
  | key == Char 'a' = ctx{ velX = -1}
  | key == Char 'd' = ctx{ velX = 1}
handleInput _ ctx = ctx

initWorld = Context (Square [(0,0),(0,-1),(-1,-1),(-1,0)]) (Square [(0,-1),(0,-2),(-1,-2),(-1,-1)]) 5 20 0 (-1) 0 (replicate 200 False) 0

