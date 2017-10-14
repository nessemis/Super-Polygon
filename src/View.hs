-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure


viewPure :: GameState -> Picture
viewPure gstate = pictures [ color white $ polygon [(10,0),(3.0,9.5),(-8.1,5.9),(-8.1,-5.9), ( 3.1 ,-9.5)] , case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])]