module View (drawContext) where

import Graphics.Gloss
import Data.List
import Structs

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