-- | This module contains the data types
--   which represent the state of the game
module Model where

import GraphicsModel

fallspeed :: Float
fallspeed = 0.1

type Player        = Float                  --(Region in terms of float).
data FallingShape  = Square Float Float     --Region (Distance bottom to floor) Height
type FallingRegion = [FallingShape]

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
initialState = GameState False False 0 [[Square 2 0.5],[],[],[],[]] (InputState False False) 0