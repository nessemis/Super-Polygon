module LevelModel where
    
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

data FallingShape  = FallingShape Float Float --(Distance bottom to floor) Height 

extractHeight :: FallingShape -> Float
extractHeight fr@(FallingShape a _) = a

type FallingRegion = [FallingShape]

data LevelState = LevelState{
    paused         :: Bool
  , player         :: Player
  , player2        :: Maybe Player
  , fallingRegions :: [FallingRegion]
  , elapsedTime    :: Float
  , score          :: Float
}

initialLevelState :: LevelState
initialLevelState = LevelState False (Player False 0 0 False) Nothing [] 0 0