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
                  
                  
movePlayer :: Player -> InputState -> Bool -> [FallingRegion] -> Player
movePlayer p InputState{keyLeft = a, keyRight = b} ishit regions
  | ishit     = p
  | a         = (p - d) `modP` regionamount 
  | b         = (p + d) `modP` regionamount 
  | otherwise = p
    where d = 0.1
          regionamount = fromIntegral $ length regions

modP :: Float -> Int -> Float
modP f i
  |f < 0     = mod' (f + fi) fi
  |otherwise = mod' f fi
    where fi = fromIntegral i
    
isHit :: Player -> [FallingRegion] -> [FallingRegion] -> Bool
isHit p oldRegions newRegions = or $ map hitTuples regionMap
  where 
        hitTuples (o, n) = (not (shapeHitPlayer o)) && (shapeHitPlayer n)             --If the previous frame, the shape didn't hit the player but this one it does, then the player is hit
        shapeHitPlayer  (FallingShape h _)  = (h <= 3)
        regionMap        = zip (currentRegion oldRegions) (currentRegion newRegions)  --Checks if any of the current regions hit the player.
        currentRegion r  = r !! (floor p)                                             --Obtains the region in which the player is from both new and old regions.