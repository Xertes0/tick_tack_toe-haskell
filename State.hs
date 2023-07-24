module State where

import Data.Sequence

windowWidth :: Float
windowWidth = 800

windowHeight :: Float
windowHeight = windowWidth

windowSize :: (Int, Int)
windowSize = (round windowWidth, round windowHeight)

data Tile =
  Empty |
  TileX |
  TileO
  deriving (Show, Eq)

type Board = Seq Tile

data Player =
  PlayerX |
  PlayerO
  deriving (Show)

data GameState =
  XTurn |
  OTurn |
  GameOver (Maybe Player)
  deriving (Show)

data State = State
  { gameState :: GameState
  , board :: Board
  } deriving (Show)
