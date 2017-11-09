module LevelModel where

import Graphics.Gloss.Data.Color
    
fallspeed :: Float
fallspeed = 1

drawingDistance :: Float
drawingDistance = 30

data Player        = Player Float Float        --(Region in terms of float) Animation.
data FallingShape  = FallingShape Float Float Color --(Distance bottom to floor) Height 
type FallingRegion = [FallingShape]

data LevelState = LevelState{
    paused         :: Bool
  , hit            :: Bool  
  , player         :: Player
  , fallingRegions :: [FallingRegion]
  , elapsedTime    :: Float
  , score          :: Float
  , speed          :: Float
}

initialLevelState :: LevelState
initialLevelState = LevelState False False (Player 0 0) [] 0 0 1 