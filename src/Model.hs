-- | This module contains the data types
--   which represent the state of the game
module Model where

import GraphicsModel

fallspeed :: Float
fallspeed = 1

type Player          = Float                  --(Region in terms of float).
data  FallingShape   = FallingShape Float Float --(Distance bottom to floor) Height 
type FallingRegion   = [FallingShape]

data InputState = InputState {
  keyLeft  :: Bool,
  keyRight :: Bool
}

data GameState = GameState {
                   menu           :: Bool
                 , hit            :: Bool
                 , player         :: Player
                 , fallingRegions :: [FallingRegion]
                 , inputState     :: InputState
                 , elapsedTime    :: Float
                 }

initialState :: GameState
initialState = GameState False False 0 [[FallingShape 3 1],[FallingShape 4 1], [FallingShape 5 2], [FallingShape 6 1]] (InputState False False) 0