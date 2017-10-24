module GameLogic (updateRegionsTick, isHit, movePlayer) where
  
import Model
import Data.Fixed

updateRegionsTick :: Float -> [FallingRegion] -> [FallingRegion]
updateRegionsTick elapsedTime regions = map (updateFallingRegion elapsedTime) regions

updateFallingRegion :: Float -> FallingRegion -> FallingRegion
updateFallingRegion elapsedTime region = map (updateFallingShape elapsedTime) region

updateFallingShape :: Float -> FallingShape -> FallingShape
updateFallingShape elapsedTime shape = case shape of
  FallingShape 0 x -> FallingShape 0 (if(translation <= 0) then 0 else translation)
            where translation = (x - fallspeed * elapsedTime)
  FallingShape d x -> FallingShape (if(translation <= 0) then 0 else translation) x
            where translation = (d - (fallspeed * elapsedTime) )
                  
                  
movePlayer :: Player -> InputState -> [FallingRegion] -> Player
movePlayer p is regions = if bp `isHit` regions then p else bp 
  where bp = moveBoundlessPlayer p is $ length regions

moveBoundlessPlayer :: Player -> InputState -> Int ->  Player
moveBoundlessPlayer p InputState{keyLeft = a, keyRight = b} regionamount
  | a         = (p - d) `modP` regionamount 
  | b         = (p + d) `modP` regionamount 
  | otherwise = p
    where d = 0.1


modP :: Float -> Int -> Float
modP f i
  |f < 0     = mod' (f + fi) fi
  |otherwise = mod' f fi
    where fi = fromIntegral i
    
isHit :: Player -> [FallingRegion] -> Bool
isHit p newRegions = or $ map shapeHitPlayer regionMap
  where 
        shapeHitPlayer (FallingShape h s) = (h <= 3 && h + s >= 3)
        regionMap                         = currentRegion newRegions --Checks if any of the current regions hit the player.
        currentRegion r                   = r !! (floor p)           --Obtains the region in which the player is from both new and old regions.