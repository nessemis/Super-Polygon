module Level where
    
import Control.Monad
import Control.Monad.IO.Class
import GameLogic

import LevelModel
import InputModel
import Model
    
initializeLevelState :: [FallingRegion] -> LevelState
initializeLevelState fr = initialLevelState {fallingRegions = fr}

updateLevelState :: Float -> InputState -> LevelState -> Caller LevelState
updateLevelState secs input lvlState =
    let 
        updatedGameState
            | paused lvlState  = lvlState {
                paused         = not $ keyPausePress input         
            }
            | hit lvlState     = lvlState {
                player         = updateDeathPlayer (player lvlState),
                fallingRegions = newRegions
            }
            | otherwise        = lvlState {
                paused         = keyPausePress input, 
                hit            = (hit lvlState ) || isHit (player lvlState) newRegions, 
                player         = newPlayer,
                fallingRegions = newRegions,
                elapsedTime    = elapsedTime lvlState + secs ,
                score          = score lvlState + secs
            }
    in
        Caller updatedGameState Nothing
    where 
        newRegions = updateRegionsTick secs (fallingRegions lvlState)
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