module GameLogic (updateRegionsTick, isHit, movePlayer) where
  
import Model

updateRegionsTick :: Float -> [FallingRegion] -> [FallingRegion]
updateRegionsTick elapsedTime regions = map (updateFallingRegion elapsedTime) regions

updateFallingRegion :: Float -> FallingRegion -> FallingRegion
updateFallingRegion elapsedTime region = map (updateFallingShape elapsedTime) region

updateFallingShape :: Float -> FallingShape -> FallingShape
updateFallingShape elapsedTime shape = case shape of
  Square d x -> FallingShape (d - (fallspeed * elapsedTime)) x

movePlayer :: Player -> InputState -> Player
movePlayer p InputState{keyLeft = a, keyRight = b}
  | a         = p - d
  | b         = p + d 
  | otherwise = p
    where d = 0.02

isHit :: Player -> [FallingRegion] -> [FallingRegion] -> Bool
isHit p oldRegions newRegions = or $ map hitTuples regionMap
  where 
        hitTuples (o, n) = (not (shapeHitPlayer o)) && (shapeHitPlayer n)             --If the previous frame, the shape didn't hit the player but this one it does, then the player is hit
        shapeHitPlayer s = case s of
                              FallingShape h _ -> (h < 0)
        regionMap        = zip (currentRegion oldRegions) (currentRegion newRegions)  --Checks if any of the current regions hit the player.
        currentRegion r  = r !! (floor p)                                             --Obtains the region in which the player is from both new and old regions.