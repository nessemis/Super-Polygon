module Level where
    
import Control.Monad
import Control.Monad.IO.Class
import GameLogic

import LevelModel
import InputModel
    
initializeLevelState :: [FallingRegion] -> LevelState
initializeLevelState fr = initialLevelState {fallingRegions = fr}

updateLevelState :: Float -> InputState -> LevelState -> LevelState
updateLevelState secs input lvlState 
    | paused lvlState  = lvlState
    | hit lvlState     = lvlState {
        player         = updateDeathPlayer (player lvlState),
        fallingRegions = newRegions
    }
    | otherwise        = lvlState {
        hit            = (hit lvlState ) || isHit (player lvlState) newRegions, 
        player         = newPlayer,
        fallingRegions = newRegions,
        elapsedTime    = elapsedTime lvlState + secs ,
        score          = score lvlState + secs
    }
  where newRegions = updateRegionsTick secs (fallingRegions lvlState)
        newPlayer  = movePlayer (player lvlState) input (newRegions)                 

-- Functions to modify the level


-- IMPURE

readLevelFile :: FilePath -> IO [FallingRegion]
readLevelFile f = do
            fileContent <- readFile f
            return (map lineToFallingRegion (lines fileContent))

lineToFallingRegion ::  String -> FallingRegion
lineToFallingRegion s = map textShapeToFallingShape (splitOn' (==',') s) 
                        
textShapeToFallingShape :: String ->  FallingShape
textShapeToFallingShape s = FallingShape (read $ head numbers) (read $ head $ tail numbers)
                        where numbers = splitOn' (==' ') s

--https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitOn'     :: (Char -> Bool) -> String -> [String]
splitOn' p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn' p s''
                            where (w, s'') = break p s'