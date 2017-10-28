-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameLogic

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | (keyPause (inputState gstate)) == True = return $ gstate
                 | otherwise =  
                          return $ gstate {
                          hit            = (hit gstate ) || isHit (player gstate) newRegions, 
                          player         = newPlayer,
                          fallingRegions = newRegions,
                          elapsedTime    = elapsedTime gstate + secs }
                            where newRegions = updateRegionsTick secs (fallingRegions gstate)
                                  newPlayer  = movePlayer (player gstate) (inputState gstate) (newRegions)                 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return gstate {inputState = inputKey e (inputState gstate)}

inputKey :: Event -> InputState -> InputState
inputKey (EventKey (SpecialKey c) d _ _) istate
  = case c of
      KeyLeft  -> istate {keyLeft  = d == Down}
      KeyRight -> istate {keyRight = d == Down}
      _        -> istate

--Check for normal keys
inputKey (EventKey (Char c) d _ _) istate
   = case c of
        'p' -> case (keyPause istate) of
                    True -> istate {keyPause = d == Down}
                    False -> istate {keyPause = d == Up}
        _   -> istate 
      
inputKey _ istate = istate -- Otherwise keep the same