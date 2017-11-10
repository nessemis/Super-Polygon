-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import LevelModel

import GraphicsLogic

view :: GameState -> IO Picture
view = return . viewPure


-- viewPure :: GameState -> Picture
-- viewPure gstate = pictures [ scale 20 20 $ rotate (-18 + (elapsedTime gstate * 100)) $ color white $ polygon [(10,0),(3.0,9.5),(-8.1,5.9),(-8.1,-5.9), ( 3.1 ,-9.5)]]

viewPure :: GameState -> Picture
viewPure gstate = scale 20 20 $ pictures [(centerPicture lState),            --Center polygon
                                          (fallingRegionsPicture lState),    --FallingRegions
                                          (playerPictures (player lState) (player2 lState) (length $ fallingRegions lState)),
                                          (scorePicture lState),
                                          (menuPicture (menuState gstate))]             --Player
    where lState = levelState gstate