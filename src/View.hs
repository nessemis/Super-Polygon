-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import LevelModel

import GraphicsModel
import GraphicsLogic

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gState@GameState{ levelState = lState, menuState = mState} = scale 20 20 $ pictures [levelPicture gState,
                                                                                         scorePicture lState,
                                                                                         menuPicture mState]
levelPicture :: GameState -> Picture
levelPicture (GameState _ _ lState (AnimationState rotationDirection rotationDirectionTarget verticalExpansion verticalExpansionTarget horizontalExpansion horizontalExpansionTarget animationTime animationTimeTarget _)) 
    = ((Rotate  rotation) . (scale scaleX scaleY)) $ pictures [fallingRegionsPicture lState, --FallingRegions
      centerPicture lState,         --Center polygon
      playerPictures (player lState) (player2 lState) (length $ fallingRegions lState)]
    where
        progress = animationTime / animationTimeTarget 
        rotation = rotationDirectionTarget * progress + rotationDirection * (1 - progress)
        scaleX   = horizontalExpansionTarget * progress + horizontalExpansion * (1 - progress)
        scaleY   = verticalExpansionTarget * progress + verticalExpansion * (1 - progress)