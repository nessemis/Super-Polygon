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
viewPure (GameState{ levelState = lState, menuState = mState}) = scale 20 20 $ pictures [centerPicture lState,             --Center polygon
                                          fallingRegionsPicture lState,                                                    --FallingRegions
                                          playerPictures (player lState) (player2 lState) (length $ fallingRegions lState),
                                          scorePicture lState,
                                          menuPicture mState]                                                               --Player