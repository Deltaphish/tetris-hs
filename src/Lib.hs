module Lib(
  game
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
, field :: [[Bool]]
, score :: Int
, time  :: Float
} deriving (Show)

projectNext :: Context -> Context
projectNext ctx = ctx {next = Square $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) (getCoords (shape ctx))}
 

checkField :: Int -> Int -> Context -> Bool
checkField col row ctx
  | col < 0 || row < 0 = True
  | otherwise = (field ctx !! row) !! col

setField :: Int -> Int -> Context -> Context
setField col row ctx = ctx{field = init rowH ++ (init colH ++ True : colT) : rowT}
  where
    (rowH,rowT) = splitAt (row+1) (field ctx)
    (colH,colT) = splitAt (col+1) (last rowH)


colPos :: (Int,Int) -> Context -> Bool
colPos (col,row) ctx
 | col < 0 || row < 0 = True
 | col >= 10 = True
 | otherwise = field ctx !! row !! col

collider :: Context -> Bool
collider ctx = True `elem` [colPos (a + posX ctx,b + posY ctx) ctx | (a,b) <- getCoords (next ctx)]


placeShape :: Context -> Context
placeShape ctx = foldr (\(a,b) ct -> setField (a + posX ctx) (b + posY ctx) ct) ctx $ getCoords $ shape ctx

spawnNew :: Context -> Context
spawnNew ctx = ctx { posY = 19, posX = 5}

moveTermino :: Context -> Context
moveTermino ctx 
  | not(collider(projectNext ctx)) = ctx { posY = posY ctx + velY ctx, posX = posX ctx + velX ctx, velX = 0, velY = -1}
  | otherwise = if velX ctx == 0 then spawnNew $ placeShape ctx else ctx {velX = 0}

standardBlock = rectangleSolid 20 20
standardBorder = rectangleWire 22 22
container = Translate 22 0 (rectangleWire (22*10) (22*20))

greyBlock = Pictures [Color (greyN 0.6) standardBlock,Color (dark yellow) standardBorder]
yellowBlock = Pictures [Color yellow standardBlock,Color (dark yellow) standardBorder]
redBlock = Pictures [Color red standardBlock,Color (dark red) standardBorder]

createPath :: Int -> Int -> Path
createPath col row = map (\(a,b) -> (a+ fromIntegral col,b + fromIntegral row)) bx where bx = [(-10,-10),(-10,10),(10,10),(10,-10)]

drawGrid :: Context -> [Picture]
drawGrid ctx = [drawRow ctx x | x <- [0..9]]

drawRow :: Context -> Int -> Picture
drawRow ctx row = Pictures [Translate (fromIntegral col*22.0 - 77) (fromIntegral row*22.0 - 22.0*9.5) greyBlock | col <- elemIndices True $ field ctx !! row]

drawContainer = container

drawShape :: Context -> [Picture]
drawShape ctx =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55) yellowBlock | let a = getCoords $ shape ctx,(col,row) <- a]

drawContext :: Context -> Picture
drawContext ctx = Pictures $ drawContainer : drawShape ctx ++ drawGrid ctx

addDeltaTime :: Float -> Context -> Context
addDeltaTime dt ctx = ctx{time = time ctx + dt}

clearDeltaTime :: Context -> Context
clearDeltaTime ctx = ctx{ time = 0.0}

stepGame :: Float -> Context -> Context
stepGame dt ctx
  | delta >= 0.5 = moveTermino $ clearDeltaTime ctx{velX = 0}
  | velX ctx /= 0 = moveTermino $ addDeltaTime dt ctx{velY = 0}
  | otherwise = addDeltaTime dt ctx
  where
    delta = time ctx

handleInput :: Event -> Context -> Context
handleInput (EventKey key st _ _) ctx
  | st == Down && key == Char 'a' = ctx{ velX = -1}
  | st == Down && key == Char 'd' = ctx{ velX = 1}
handleInput _ ctx = ctx

initWorld = Context (Square [(0,0),(0,-1),(-1,-1),(-1,0)]) (Square [(0,-1),(0,-2),(-1,-2),(-1,-1)]) 5 19 0 (-1) 0 (replicate 20 (replicate 10 False)) 0 0.0
game = play (InWindow "Nice Window" (500,700) (10,10)) white 60 initWorld drawContext handleInput stepGame

