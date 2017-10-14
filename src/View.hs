-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

import GraphicsLogic

view :: GameState -> IO Picture
view = return . viewPure


-- viewPure :: GameState -> Picture
-- viewPure gstate = pictures [ scale 20 20 $ rotate (-18 + (elapsedTime gstate * 100)) $ color white $ polygon [(10,0),(3.0,9.5),(-8.1,5.9),(-8.1,-5.9), ( 3.1 ,-9.5)]]

viewPure :: GameState -> Picture
viewPure gstate = scale 20 20 $ pictures [(centerPicture gstate), (playerPicture gstate), (fallingRegionsPicture gstate) ]