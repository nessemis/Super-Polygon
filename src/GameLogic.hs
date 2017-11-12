module GameLogic (updateRegionsTick, isHit, movePlayer, updateDeathPlayer, modP) where
  
import LevelModel
import InputModel
import Data.Fixed

dx :: Float
dx = 0.09

updateRegionsTick :: Float -> Float -> [FallingRegion] -> [FallingRegion]
updateRegionsTick speed elapsedTime fallingRegions = map (updateFallingRegion speed elapsedTime) fallingRegions

updateFallingRegion :: Float -> Float -> FallingRegion -> FallingRegion
updateFallingRegion speed elapsedTime fallingRegions = map (updateFallingShape speed elapsedTime) fallingRegions

updateFallingShape :: Float ->  Float -> FallingShape -> FallingShape
updateFallingShape speed elapsedTime shape = case shape of
  FallingShape 0 height color -> FallingShape 0 (if updatedHeight <= 0 then 0 else updatedHeight) color
            where updatedHeight = (height - speed * elapsedTime)
  FallingShape distance height color -> FallingShape (if updatedDistance <= 0 then 0 else updatedDistance) height color
            where updatedDistance = (distance - (speed * elapsedTime) )
                  
                  
movePlayer :: Player -> PlayerMovement -> [FallingRegion] -> Float
movePlayer player playerMovement fallingRegions = if updatedPlayer `isHit` fallingRegions then location player else location updatedPlayer 
    where updatedPlayer = moveBoundlessPlayer player playerMovement $ length fallingRegions

--Update player animation and register pause
updateDeathPlayer :: Player -> Player
updateDeathPlayer p = p {deathAnimation = deathAnimation p + 1}

moveBoundlessPlayer :: Player -> PlayerMovement -> Int ->  Player
moveBoundlessPlayer player@(Player {location = location}) playerMovement regionamount = case playerMovement of
    MoveLeft  -> player {location = (location - dx) `modP` regionamount }
    MoveRight -> player {location = (location + dx) `modP` regionamount }
    otherwise -> player
              
modP :: Float -> Int -> Float
modP f i
  |f < 0     = mod' (f + fi) fi
  |otherwise = mod' f fi
    where fi = fromIntegral i
    
isHit :: Player -> [FallingRegion] -> Bool
isHit player newRegions = or $ map shapeHitPlayer regionMap
  where 
       shapeHitPlayer (FallingShape distance height _) = distance <= 3 && distance + height >= 3
       regionMap                         = currentRegion newRegions --Checks if any of the current regions hit the player.
       currentRegion r                   = r !! (floor $ location player)           --Obtains the region in which the player is from both new and old regions.
