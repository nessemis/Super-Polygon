module GameLogic (updateRegionsTick, isHit, movePlayer, updateDeathPlayer, modP) where
  
import LevelModel
import InputModel
import Data.Fixed

dx :: Float
dx = 0.1

updateRegionsTick :: Float -> Float -> [FallingRegion] -> [FallingRegion]
updateRegionsTick speed elapsedTime regions = map (updateFallingRegion speed elapsedTime) regions

updateFallingRegion :: Float -> Float -> FallingRegion -> FallingRegion
updateFallingRegion speed elapsedTime region = map (updateFallingShape speed elapsedTime) region

updateFallingShape :: Float ->  Float -> FallingShape -> FallingShape
updateFallingShape speed elapsedTime shape = case shape of
  FallingShape 0 x c -> FallingShape 0 (if(translation <= 0) then 0 else translation) c
            where translation = (x - speed * elapsedTime)
  FallingShape d x c -> FallingShape (if(translation <= 0) then 0 else translation) x c
            where translation = (d - (speed * elapsedTime) )
                  
                  
movePlayer :: Player -> PlayerMovement -> [FallingRegion] -> Float
movePlayer p pm regions = if bp `isHit` regions then location p else location bp 
    where bp = moveBoundlessPlayer p pm $ length regions

--Update player animation and register pause
updateDeathPlayer :: Player -> Player
updateDeathPlayer p = p {deathAnimation = deathAnimation p + 1}

moveBoundlessPlayer :: Player -> PlayerMovement -> Int ->  Player
moveBoundlessPlayer player@(Player {location = p}) pm regionamount = case pm of
    MoveLeft  -> player {location = (p - dx) `modP` regionamount }
    MoveRight -> player {location = (p + dx) `modP` regionamount }
    otherwise -> player
              
    
    
modP :: Float -> Int -> Float
modP f i
  |f < 0     = mod' (f + fi) fi
  |otherwise = mod' f fi
    where fi = fromIntegral i
    
isHit :: Player -> [FallingRegion] -> Bool
isHit p newRegions = or $ map shapeHitPlayer regionMap
  where 
       shapeHitPlayer (FallingShape h s _) = (h <= 3 && h + s >= 3)
       regionMap                         = currentRegion newRegions --Checks if any of the current regions hit the player.
       currentRegion r                   = r !! (floor (location p))           --Obtains the region in which the player is from both new and old regions.
