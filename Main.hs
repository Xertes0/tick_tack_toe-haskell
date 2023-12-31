module Main where

import Drawing
import State
import Logic

import Data.Sequence
import Graphics.Gloss

initialState :: State
initialState =
  State
    { gameState = XTurn
    , board = fromList [State.Empty | _ <- [0 .. (3 * 3) - 1]]
    }

main :: IO ()
main =
  play
    (InWindow "Tick tack toe" windowSize (100, 100))
    white
    30
    initialState
    makePicture
    handleEvent
    (const id)
