module Model(play,initWorld,handleInput,stepGame) where
  
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import qualified System.Random
import Structs

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
spawnNew ctx = (chooseShape ctx) { posY = 19, posX = 4, score = score ctx + 1}

moveTermino :: Context -> Context
moveTermino ctx 
  | not(collider(projectNext ctx (next (Model.rotate ctx (shape ctx))))) = ctx { shape = next $ Model.rotate ctx (shape ctx) ,posY = posY ctx + velY ctx, posX = posX ctx + velX ctx, velX = 0, velY = -1, rot = 0}
  | otherwise = if velX ctx == 0 && rot ctx == 0 then spawnNew $ findCompleteLines $ placeShape ctx else ctx {velX = 0,rot = 0}


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




