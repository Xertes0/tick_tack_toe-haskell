{-# LANGUAGE RecordWildCards #-}

module Drawing where

import State
import Data.Foldable
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

drawGrid :: Picture
drawGrid = pictures $ fold $ [ map line [ [ (windowWidth / 3.0 * i, 0.0)
                                          , (windowWidth / 3.0 * i, windowHeight) ]
                                        , [ (0.0,                   windowHeight / 3.0 * i)
                                          , (windowWidth,           windowHeight / 3.0 * i) ]
                                        ] | i <- [1..2] ]

xWidth :: Float
xWidth = 20.0

xHeight :: Float
xHeight = 200.0

drawX :: Picture
drawX = color blue cross
  where
    cross = rotate 45.0 $ pictures [ rectangleSolid xWidth xHeight
                                   , rotate 90.0 $ rectangleSolid xWidth xHeight]

oThickness :: Float
oThickness = xWidth

oRad :: Float
oRad = 90.0

drawO :: Picture
drawO = color red circle
  where
    circle = thickCircle oRad oThickness

drawTile :: Tile -> Picture
drawTile TileX = drawX
drawTile TileO = drawO
drawTile _ = error "Trying to draw an undrawable Tile"

toFloat :: Int -> Float
toFloat x = fromIntegral x

winnerColor :: Maybe Player -> Color
winnerColor Nothing = greyN 0.5
winnerColor (Just PlayerX) = blue
winnerColor (Just PlayerO) = red

makePicture :: State -> Picture
makePicture State{..} = case gameState of
                          GameOver winner -> color (winnerColor winner) output
                          _ -> output
  where
    output =
      pictures
      $ map (translate (-windowWidth / 2.0) (-windowHeight / 2.0))
      $ (++ [drawGrid])
      $ map (\(t,x,y) -> translate
                         (windowWidth / 3.0 * (toFloat x) - windowWidth / 6.0)
                         (windowHeight / 3.0 * (toFloat y) - windowHeight / 6.0)
                         $ drawTile t)
      $ filter (\(t,_,_) -> t /= Empty)
      $ zip3 (toList board) [ (i `mod` 3) + 1 | i <- [0..3*3-1] ] (fold [ [ i | _ <- [0..2] ] | i <- [1..3] ])
