module Lib(
  game
, shape
, chooseShape
, initWorld
, stepGame
, Lib.rotate
, rot
, rotateMatrix
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import qualified System.Random


data Shape = Square [(Int,Int)] 
           | Lblock [(Int,Int)] 
           | RLblock [(Int,Int)] 
           | Squig [(Int,Int)] 
           | RSquig [(Int,Int)] 
           | Tblock [(Int,Int)]
           | Straight [(Int,Int)] deriving (Show)

square = Square [(0,0),(1,0),(1,-1),(0,-1)]
lblock = Lblock [(0,0),(0,1),(0,-1),(1,-1)]
rblock = RLblock [(0,0),(0,1),(0,-1),(-1,-1)]
squig = Squig [(0,0),(1,0),(-1,-1),(0,-1)]
rsquig = RSquig [(0,0),(-1,0),(1,-1),(0,-1)]
straight = Straight [(0,0),(-1,0),(1,0),(2,0)]
tblock = Tblock [(0,0),(1,0),(-1,0),(0,1)]

chooseShapeList = [square,lblock,rblock,square,rsquig,straight,tblock]

chooseShape :: Context -> Context
chooseShape ctx = newCtx{shape = chooseShapeList !! rnd}
  where
    (n,gen) = System.Random.next (generator ctx)
    rnd = n `mod` 7
    newCtx = ctx{generator=gen}



getCoords :: Shape -> [(Int,Int)]
getCoords (Square arr) = arr
getCoords (Lblock arr) = arr
getCoords (RLblock arr) = arr
getCoords (Squig arr) = arr
getCoords (RSquig arr) = arr
getCoords (Straight arr) = arr
getCoords (Tblock arr) = arr

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
} deriving (Show)

projectNext :: Context -> Shape-> Context
projectNext ctx (Square coords)= ctx {next = Square $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) coords}
projectNext ctx (Lblock coords)= ctx {next = Lblock $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) coords}
projectNext ctx (RLblock coords)= ctx {next = RLblock $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) coords}
projectNext ctx (Squig coords)= ctx {next = Squig $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) coords}
projectNext ctx (RSquig coords)= ctx {next = RSquig $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) coords}
projectNext ctx (Straight coords)= ctx {next = Straight $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) coords}
projectNext ctx (Tblock coords)= ctx {next = Tblock $ map(\(a,b) -> (a + velX ctx,b + velY ctx)) coords}


rotateMatrix :: (Int,Int) -> Int -> (Int,Int)
rotateMatrix (a,b) 0 = (a,b)
rotateMatrix (a,b) i = rotateMatrix (b,-a) (i - 1)

rotate :: Context -> Shape -> Context
rotate ctx (Square coords)= ctx{next = Square $ map(\(a,b) -> rotateMatrix (a,b) (rot ctx)) coords}
rotate ctx (Lblock coords)= ctx{next = Lblock $ map(\(a,b) -> rotateMatrix (a,b) (rot ctx)) coords}
rotate ctx (RLblock coords)= ctx{next = RLblock $ map(\(a,b) -> rotateMatrix (a,b) (rot ctx)) coords}
rotate ctx (Squig coords)= ctx{next = Squig $ map(\(a,b) -> rotateMatrix (a,b) (rot ctx)) coords}
rotate ctx (RSquig coords)= ctx{next = RSquig $ map(\(a,b) -> rotateMatrix (a,b) (rot ctx)) coords}
rotate ctx (Straight coords)= ctx{next = Straight $ map(\(a,b) -> rotateMatrix (a,b) (rot ctx)) coords}
rotate ctx (Tblock coords)= ctx{next = Tblock $ map(\(a,b) -> rotateMatrix (a,b) (rot ctx)) coords}


checkField :: Int -> Int -> Context -> Bool
checkField col row ctx
  | col < 0 || row < 0 = True
  | otherwise = (field ctx !! row) !! col

setField :: Int -> Int -> Context -> Context
setField col row ctx = ctx{field = init rowH ++ (init colH ++ True : colT) : rowT}
  where
    (rowH,rowT) = splitAt (row+1) (field ctx)
    (colH,colT) = splitAt (col+1) (last rowH)

expandList :: [[Bool]] -> [[Bool]]
expandList arr | length arr < 21 = expandList (arr ++ [replicate 10 False])
               | otherwise = arr

findComplete :: [[Bool]] -> [[Bool]]
findComplete [] = []
findComplete arr | and $ head arr = findComplete $ tail arr
                 | otherwise = head arr : findComplete (tail arr)

findCompleteLines :: Context -> Context
findCompleteLines ctx = ctx{field = expandList $ findComplete $ field ctx}

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
spawnNew ctx = (chooseShape ctx) { posY = 19, posX = 4, score = (score ctx) + 1}

moveTermino :: Context -> Context
moveTermino ctx 
  | not(collider(projectNext ctx (next (Lib.rotate ctx (shape ctx))))) = ctx { shape = next $ Lib.rotate ctx (shape ctx) ,posY = posY ctx + velY ctx, posX = posX ctx + velX ctx, velX = 0, velY = -1, rot = 0}
  | otherwise = if velX ctx == 0 && rot ctx == 0 then spawnNew $ findCompleteLines $ placeShape ctx else ctx {velX = 0,rot = 0}

standardBlock = rectangleSolid 20 20
standardBorder = rectangleWire 22 22
container = Translate 22 0 (rectangleWire (22*10) (22*20))

greyBlock = Pictures [Color (greyN 0.6) standardBlock,Color (dark yellow) standardBorder]
yellowBlock = Pictures [Color yellow standardBlock,Color (dark yellow) standardBorder]
redBlock = Pictures [Color red standardBlock,Color (dark red) standardBorder]
cyanBlock = Pictures [Color cyan standardBlock,Color (dark cyan) standardBorder]
orangeBlock = Pictures [Color orange standardBlock,Color (dark orange) standardBorder]
purpleBlock = Pictures [Color violet standardBlock,Color (dark violet) standardBorder]
blueBlock = Pictures [Color blue standardBlock,Color (dark blue) standardBorder]
greenBlock = Pictures [Color green standardBlock,Color (dark green) standardBorder]

createPath :: Int -> Int -> Path
createPath col row = map (\(a,b) -> (a+ fromIntegral col,b + fromIntegral row)) bx where bx = [(-10,-10),(-10,10),(10,10),(10,-10)]

drawGrid :: Context -> [Picture]
drawGrid ctx = [drawRow ctx x | x <- [0..19]]

drawRow :: Context -> Int -> Picture
drawRow ctx row = Pictures [Translate (fromIntegral col*22.0 - 77) (fromIntegral row*22.0 - 22.0*9.5) greyBlock | col <- elemIndices True $ field ctx !! row]

drawContainer = container

drawShape :: Context -> Shape -> [Picture]
drawShape ctx (Square coords) =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55) yellowBlock | (col,row) <- coords]
drawShape ctx (Lblock coords) =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55)  orangeBlock | (col,row) <- coords]
drawShape ctx (RLblock coords) =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55) blueBlock | (col,row) <- coords]
drawShape ctx (Squig coords) =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55) greenBlock | (col,row) <- coords]
drawShape ctx (RSquig coords) =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55) redBlock | (col,row) <- coords]
drawShape ctx (Straight coords) =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55) cyanBlock | (col,row) <- coords]
drawShape ctx (Tblock coords) =  [Translate (fromIntegral (col+posX ctx)*22.0 - 77) (fromIntegral (row+posY ctx)*22.0 - 22.0*7.0 - 55) purpleBlock | (col,row) <- coords]


drawScore :: Context -> Picture
drawScore ctx = Scale 0.5 0.5 $ Translate (-500.0) (-22.0) (Text $ show $ score ctx)

drawContext :: Context -> Picture
drawContext ctx = Pictures $ drawScore ctx : drawContainer : drawShape ctx (shape ctx) ++ drawGrid ctx

addDeltaTime :: Float -> Context -> Context
addDeltaTime dt ctx = ctx{time = time ctx + dt}

clearDeltaTime :: Context -> Context
clearDeltaTime ctx = ctx{ time = 0.0}

stepGame :: Float -> Context -> Context
stepGame dt ctx
  | delta >= 0.25 = checkForLoss $ moveTermino $ clearDeltaTime ctx{velX = 0}
  | velX ctx /= 0 = moveTermino $ addDeltaTime dt ctx{velY = 0}
  | otherwise = addDeltaTime dt ctx
  where
    delta = time ctx

handleInput :: Event -> Context -> Context
handleInput (EventKey key st _ _) ctx
  | st == Down && key == Char 'a' = ctx{ velX = -1}
  | st == Down && key == Char 'd' = ctx{ velX = 1}
  | st == Down && key == Char 's' = ctx{ rot = 1}
handleInput _ ctx = ctx

initWorld :: System.Random.StdGen -> Context
initWorld = Context (Square [(0,0),(0,-1),(-1,-1),(-1,0)]) (Square [(0,-1),(0,-2),(-1,-2),(-1,-1)]) 5 20 0 (-1) 0 (replicate 21 (replicate 10 False)) 0 0.0 

checkForLoss :: Context -> Context
checkForLoss ctx
  | or (field ctx !! 19) = ctx{field = replicate 21 $ replicate 10 False, score = 0}
  | otherwise = ctx


game xs = play (InWindow "Nice Window" (500,700) (10,10)) white 60 (initWorld xs) drawContext handleInput stepGame


