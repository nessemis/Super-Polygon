-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameLogic

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

import MenuModel
import InputModel
import LevelModel


import Menu
import Input
import Level

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
{- step secs gstate 
                 | (menu gstate) = 
                          return $ gstate {
                          menu           = not (keyEnter (inputState gstate))} 
                 --Pause key pressed   
                 | (keyPause (inputState gstate)) = return $ gstate
                 --GAMEOVER
                 | (hit gstate) =
                          return $ gstate {
                          fallingRegions = newRegions,
                          player         = updateDeathPlayer (player gstate),
                          inputState     = (inputState gstate),
                          elapsedTime    = elapsedTime gstate + secs}
                 --Normal Gameplay
                 | otherwise =  
                          return $ gstate {
                          menu           = (keyEscape (inputState gstate)),
                          hit            = (hit gstate ) || isHit (player gstate) newRegions, 
                          player         = newPlayer,
                          fallingRegions = newRegions,
                          elapsedTime    = elapsedTime gstate + secs ,
                          score          = score gstate + secs }
                            where newRegions = updateRegionsTick secs (fallingRegions gstate)
                                  newPlayer  = movePlayer (player gstate) (inputState gstate) (newRegions)                 
-}

step secs gstate = return gstate {
  inputState = updateInputState (inputState gstate),
  menuState  = updateMenuState (inputState gstate) (menuState gstate),
  levelState = updateLevelState secs (inputState gstate) (levelState gstate)
}

--temporary, for starting with a loaded level

initializedState :: [FallingRegion] -> GameState
initializedState fr = GameState initialInputState initialMenuState (initializeLevelState fr)

----------------------------------------------

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return gstate {inputState = inputKey e (inputState gstate)}