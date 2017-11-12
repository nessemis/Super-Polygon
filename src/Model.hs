-- | This module contains the data types
--   which represent the state of the game
module Model where

--import GraphicsModel
import InputModel
import MenuModel
import LevelModel
import GraphicsModel

data GameState = GameState {
    inputState :: InputState                  
  , menuState :: MenuState
  , levelState :: LevelState
  , animationState :: AnimationState
}

data Caller a = Caller a (Maybe Call) --The caller and the call

data Call = StartLevel LevelParameters | ShowMenu | ResumeGame | EndGame String | QuitGame --The calls that can be made
