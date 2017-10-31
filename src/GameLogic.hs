module GameLogic (updateRegionsTick, isHit, movePlayer, updateDeathPlayer) where
  
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

--Update player animation and register pause
updateDeathPlayer :: Player -> Player
updateDeathPlayer (Player p aq) = Player p (aq+1)

moveBoundlessPlayer :: Player -> InputState -> Int ->  Player
moveBoundlessPlayer (Player p aq) InputState{keyLeft = a, keyRight = b} regionamount
    | a         = Player ((p - d) `modP` regionamount) aq  
    | b         = Player ((p + d) `modP` regionamount) aq
    | otherwise = Player p aq
        where d = 0.1
              
    
    
modP :: Float -> Int -> Float
modP f i
  |f < 0     = mod' (f + fi) fi
  |otherwise = mod' f fi
    where fi = fromIntegral i
    
isHit :: Player -> [FallingRegion] -> Bool
isHit (Player p aq) newRegions = or $ map shapeHitPlayer regionMap
  where 
       shapeHitPlayer (FallingShape h s) = (h <= 3 && h + s >= 3)
       regionMap                         = currentRegion newRegions --Checks if any of the current regions hit the player.
       currentRegion r                   = r !! (floor p)           --Obtains the region in which the player is from both new and old regions.