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
import GraphicsModel


import Menu
import Input
import Level

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gState@(GameState iState mState lState aState) =
  let 
      updatedGStateGstate = gState {
        inputState = updatedInputState,
        menuState  = updatedMenuState,
        levelState = updatedLevelState,
        animationState = updatedAnimationState
      }
  in
    return updatedGStateGstate >>= handleCall call1 >>= handleCall call2 >>= handleCall call3
  where
    (Caller updatedInputState call1) = updateInputState iState
    (Caller updatedMenuState call2)  = updateMenuState iState mState
    (Caller updatedLevelState call3) = updateLevelState secs iState lState
    updatedAnimationState            = updateLevelAnimation secs aState

handleCall :: Maybe Call -> GameState -> IO GameState
handleCall Nothing gState = return gState
handleCall (Just call) gState@GameState{ menuState = mState, levelState = lState} = 
  case call of
    QuitGame  -> exitWith ExitSuccess    
    StartLevel parameters -> do
                               updatedLState <- startLevel parameters
                               return $ gState{ levelState = updatedLState, menuState = mState{visible = False}}
    EndGame message -> return $ gState { levelState = lState{paused = True}, menuState = endGameMenuState message . extractLevelSelectScreen . screen $ mState}
    ShowMenu   -> if visible (mState) then return gState else
                    return gState{ menuState = mState{ visible = True, screen = extractLevelSelectScreen (screen mState) }, levelState = lState{ paused = True} }
    ResumeGame -> if not $ paused lState || isGameFinished lState then return gState else
                      return gState{ menuState = mState{ visible = False }, levelState = lState{ paused = False} }                      

getLevelPaths :: IO [String]
getLevelPaths = do
              levelPaths <- listDirectory "levels\\"
              return $ map (reverse . (trimHeadString 4) . reverse) levelPaths

trimHeadString 0 x = x
trimHeadString n (x:xs) = trimHeadString (n - 1) xs



initializeState :: ([FallingRegion], Float,Float) -> [String] -> Int -> GameState
initializeState fallingRegions levelPaths seed = GameState initialInputState (initialMenuState levelPaths) (initializeLevelState fallingRegions){paused = True} (initialAnimationState seed)

----------------------------------------------

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gState@GameState{ inputState = iState } = return gState{inputState = inputKey e iState}