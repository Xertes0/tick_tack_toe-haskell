{-# LANGUAGE RecordWildCards #-}

module Logic where

import State
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Sequence as Seq
import Graphics.Gloss.Interface.Pure.Game

placeTile :: (Float, Float) -> State -> State
placeTile (mouseX, mouseY) state =
  if canPlace
  then state { board = Seq.update boardIndex tileToPlace (board state)
             , gameState = newGameState }
  else state
  where
    (clickX, clickY) = ( mouseX + (windowWidth / 2.0)
                       , mouseY + (windowHeight / 2.0) )
    boardX = floor $ clickX / (windowWidth / 3.0)
    boardY = floor $ clickY / (windowHeight / 3.0)
    boardIndex = boardY * 3 + boardX
    canPlace = State.Empty == Seq.index (board state) boardIndex
    tileToPlace = case gameState state of
      XTurn -> TileX
      OTurn -> TileO
      _ -> error "tileToPlace error"
    newGameState = case gameState state of
      XTurn -> OTurn
      OTurn -> XTurn
      _ -> error "tileToPlace error"

listWon :: [Tile] -> Maybe Player
listWon [TileX,TileX,TileX] = Just PlayerX
listWon [TileO,TileO,TileO] = Just PlayerO
listWon x = Nothing

checkGameOver :: State -> State
checkGameOver state | isJust winner = state { gameState = GameOver winner }
                    | otherwise = state
  where
    wonInRow =
      chunksOf 3
      $ toList $ board state
    wonInCol =
      transpose
      $ chunksOf 3
      $ toList $ board state
    wonInCross =
      map (map (\x -> Seq.index (board state) (x-1))) [ [1,5,9], [3,5,7] ]
    winner =
      listToMaybe
      $ map fromJust
      $ filter (\x -> not $ isNothing x)
      $ map (\x -> listWon $ toList x)
      $ wonInRow ++ wonInCol ++ wonInCross

handleEvent :: Event -> State -> State
handleEvent event state
  | EventKey (MouseButton LeftButton) Down _ (mouseX,mouseY) <- event
  = case gameState state of
      GameOver _ -> state
      _ -> checkGameOver $ placeTile (mouseX, mouseY) state
  | otherwise = state
