module Level where
    
import Data.Maybe
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
            | hit p1 = lvlState {
                player         = updateDeathPlayer p1,
                fallingRegions = newRegions
            }
            | isJust p2 && hit jp2 = lvlState {
                player2        = Just (updateDeathPlayer jp2),
                fallingRegions = newRegions
            }
            | otherwise        = lvlState {
                paused         = keyPausePress input, 
                player         = p1 { hit = hit p1 || isHit p1 newRegions,
                                      location = newPlayerLocation},
                fallingRegions = newRegions,
                elapsedTime    = elapsedTime lvlState + secs ,
                score          = score lvlState + secs,
                player2        = if isJust p2 then 
                    Just jp2{ hit      = hit jp2 || isHit jp2 newRegions,
                         location = newPlayer2Location}
                    else p2

            }
    in
        Caller updatedGameState Nothing
    where
        p1         = player lvlState
        p2         = player2 lvlState
        jp2        = fromJust p2
        newRegions = updateRegionsTick secs (fallingRegions lvlState)
        newPlayerLocation  = movePlayer p1 (inputToMovement input) newRegions
        newPlayer2Location = case p2 of
            Just p@(Player _ _ _ False)  -> movePlayer p (inputToMovement2 input) newRegions
            Just p@(Player _ _ _ True)   -> movePlayer p (movementAi lvlState) newRegions

inputToMovement :: InputState -> PlayerMovement
inputToMovement is 
                | keyLeft is  = MoveLeft
                | keyRight is = MoveRight
                | otherwise   = Idle

inputToMovement2 :: InputState -> PlayerMovement
inputToMovement2 is 
                | keyA is  = MoveLeft
                | keyD is = MoveRight
                | otherwise   = Idle

movementAi :: LevelState -> PlayerMovement
movementAi lvlState = closestRegionLocation (fromJust (player2 lvlState)) (fallingRegions lvlState)

closestRegionLocation :: Player -> [FallingRegion] -> PlayerMovement
closestRegionLocation p r 
    | abs distance <= 0.2                                           = Idle
    | distance >= 0 && distance <= ((fromIntegral regionCount) / 2) = MoveRight
    | distance >= 0 && distance >= ((fromIntegral regionCount) / 2) = MoveLeft
    | distance <= 0 && distance <= ((fromIntegral regionCount) / 2) = MoveRight
    | distance <= 0 && distance >= ((fromIntegral regionCount) / 2) = MoveLeft
    where closestRegionLocation = (extractHeight (head (findLowest (filter ((<=) 3 . extractHeight) (map head r)))) + 0.5) `modP` regionCount
          regionCount           = (length r)
          distance              = closestRegionLocation - location p
          
findLowest :: [FallingShape] -> [FallingShape]
findLowest [x]    = [x]
findLowest (x:xs) = let y = head xs in
                        if extractHeight x < extractHeight y
                            then findLowest $ x:(tail xs) else findLowest xs

startLevel :: LevelOptions -> IO LevelState
startLevel options = do
                        x <- case (randomOrLoad options) of
                                Right path -> readLevelFile path
                                Left int -> undefined
                        return $ initializeLevelState x
        
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