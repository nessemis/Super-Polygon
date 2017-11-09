module LevelModel where

import Graphics.Gloss.Data.Color
    
fallspeed :: Float
fallspeed = 1

drawingDistance :: Float
drawingDistance = 30

data Player = Player {
                hit            :: Bool  
              , location       :: Float
              , deathAnimation :: Float    
              , ai             :: Bool                  
              }
                
data PlayerMovement = Idle | MoveLeft | MoveRight

data FallingShape  = FallingShape Float Float Color--(Distance bottom to floor) Height 

extractHeight :: FallingShape -> Float
extractHeight fr@(FallingShape a _ _) = a

type FallingRegion = [FallingShape]

data LevelState = LevelState{
    paused         :: Bool
  , player         :: Player
  , player2        :: Maybe Player
  , fallingRegions :: [FallingRegion]
  , elapsedTime    :: Float
  , score          :: Float
  , speed          :: Float
}

initialLevelState :: LevelState
initialLevelState = LevelState False (Player False 0 0 False) Nothing [] 0 0 0
