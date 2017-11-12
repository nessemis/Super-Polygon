-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameLogic

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Exit
import System.Directory

import MenuModel
import InputModel
import LevelModel


import Menu
import Input
import Level

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
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
    QuitGame  -> exitWith ExitSuccess    
    StartLevel options -> do
                            ls <- startLevel options
                            gss <- gs
                            return $ gss {levelState = ls, menuState = (menuState gss){visible = False}}
    EndGame m -> do
                  gss <- gs
                  return $ gss {levelState = (levelState gss){paused = True}, menuState = endGameMenuState m (extractLevelSelectScreen (screen (menuState gss)))}
    ShowMenu  -> do
                  gss <- gs
                  if visible (menuState gss) then return gss else
                    return gss{menuState = (menuState gss){ visible = True }, levelState = (levelState gss){ paused = True} }
    ResumeGame -> do
                    gss <- gs
                    if not $ paused (levelState gss) then return gss else
                      return gss{menuState = (menuState gss){ visible = False }, levelState = (levelState gss){ paused = False} }                      

--temporary, for starting with a loaded level

getLevels :: IO [String]
getLevels = do
              levelPaths <- listDirectory "levels\\"
              return $ map (reverse . (trimHeadString 4) . reverse) levelPaths

trimHeadString 0 string = string
trimHeadString n (x:xs) = trimHeadString (n - 1) xs

initializedState :: ([FallingRegion],Float) -> [String] -> GameState
initializedState fr levels = GameState initialInputState (initialMenuState levels) (initializeLevelState fr){paused = True}

----------------------------------------------

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return gstate {inputState = inputKey e (inputState gstate)}