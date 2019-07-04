module App (game) where
import View
import Model
import Graphics.Gloss

game xs = play (InWindow "Nice Window" (500,700) (10,10)) white 60 (initWorld xs) drawContext handleInput stepGame
