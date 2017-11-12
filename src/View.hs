-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import LevelModel

import GraphicsLogic

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState{ levelState = lState, menuState = mState}) = scale 20 20 $ pictures [levelPicture,
                                                                                         scorePicture lState,
                                                                                         menuPicture mState]
    where levelPicture = pictures [fallingRegionsPicture lState, --FallingRegions
                                   centerPicture lState,         --Center polygon
                                   playerPictures (player lState) (player2 lState) (length $ fallingRegions lState)]