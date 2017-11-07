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

data Caller a = Caller a (Maybe Call) --The caller and the call

data Call = StartLevel LevelOptions | ShowMenu | QuitGame

data LevelOptions = LevelOptions {
    randomOrLoad :: Either Int String, --int is the seed, string the path
    twoPlayers :: Bool,
    ai         :: Bool
}

initialState :: GameState
initialState = GameState initialInputState initialMenuState initialLevelState
