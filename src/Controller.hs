-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameLogic

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Exit

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

step secs gstate =
  let 
      updatedGStateGstate = gstate {
        inputState = updatedInputState,
        menuState  = updatedMenuState,
        levelState = updatedLevelState
      }
  in
    handleCall call1 $ handleCall call2 $ handleCall call3 (return updatedGStateGstate)
  where
    (Caller updatedInputState call1) = updateInputState (inputState gstate)
    (Caller updatedMenuState call2) = updateMenuState (inputState gstate) (menuState gstate)
    (Caller updatedLevelState call3) = updateLevelState secs (inputState gstate) (levelState gstate)

handleCall :: Maybe Call -> IO GameState -> IO GameState
handleCall Nothing gs = gs
handleCall (Just call) gs = 
  case call of
    StartLevel options -> do
                            ls <- startLevel options
                            gss <- gs
                            return $ gss {levelState = ls, menuState = (menuState gss){visible = False}}
    QuitGame -> do
                  gss <- gs
                  case visible((menuState )gss ) of
                    True -> exitWith ExitSuccess
                    False-> return gss{menuState = (menuState gss){ visible = True } }
    EndGame m -> do
                  gss <- gs
                  return $ gss {levelState = (levelState gss){paused = True}, menuState = endGameMenuState m }
    otherwise -> gs

--temporary, for starting with a loaded level

initializedState :: ([FallingRegion],Float) -> GameState
initializedState fr = GameState initialInputState initialMenuState (initializeLevelState fr)

----------------------------------------------

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return gstate {inputState = inputKey e (inputState gstate)}