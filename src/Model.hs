-- | This module contains the data types
--   which represent the state of the game
module Model where

--import GraphicsModel
import InputModel
import MenuModel
import LevelModel

data GameState = GameState {
    inputState :: InputState                     
  , menuState :: MenuState
  , levelState :: LevelState
}

initialState :: GameState
initialState = GameState initialInputState initialMenuState initialLevelState
