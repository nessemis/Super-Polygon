module Level where
    
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import GameLogic

import LevelModel
import InputModel
import Model

import Graphics.Gloss.Data.Color
    
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
        newPlayerLocation  = case p1 of
            (Player _ _ _ False) -> movePlayer p1 (inputToMovement input) newRegions
            (Player _ _ _ True)  -> movePlayer p1 (movementAi lvlState p1) newRegions            
        newPlayer2Location = case p2 of
            Just p@(Player _ _ _ False)  -> movePlayer p (inputToMovement2 input) newRegions
            Just p@(Player _ _ _ True)   -> movePlayer p (movementAi lvlState jp2) newRegions

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

movementAi :: LevelState -> Player -> PlayerMovement
movementAi lvlState p = closestRegionLocation p (fallingRegions lvlState)

closestRegionLocation :: Player -> [FallingRegion] -> PlayerMovement
closestRegionLocation p r 
    | abs direction <= 0.2 = Idle
    | direction >= 0       = MoveRight
    | direction <= 0       = MoveLeft
    where preferredLocation = fromIntegral (fst (head (findHighest (zip [0..] (map firstCollidableRegion r))))) + 0.5
          halfRegions       = (fromIntegral (length r)) / 2
          distance          = preferredLocation - location p
          direction         = if abs distance >= halfRegions then abs distance / distance * (-2) * halfRegions + distance else distance
          
firstCollidableRegion :: [FallingShape] -> FallingShape
firstCollidableRegion region  
    | length region == 0               = FallingShape 100 1 (black)
    | extractHeight (head region) <= 3 = firstCollidableRegion (tail region)
    | otherwise                        = head region

findHighest :: [(Int, FallingShape)] -> [(Int, FallingShape)]
findHighest [x]    = [x]
findHighest (x:xs) = let y = head xs in
            if (extractHeight . snd) x > (extractHeight . snd) y
                then findHighest $ x:(tail xs) else findHighest xs

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
textShapeToFallingShape s = FallingShape (read $ head numbers) 
                                         (read $ head $ tail numbers) 
                                         (toColor (read $ head $ tail $ tail numbers))
                        where numbers = splitOn' (==' ') s

toColor :: Int -> Color
toColor 0 = makeColor 1 0 0 1 --RED
toColor 1 = makeColor 1 1 0 1 --YELLOW
toColor 3 = makeColor 1 0 1 1 --PINK
toColor 4 = makeColor 0 1 1 1 --BLUEGREEN
toColor 5 = makeColor 0 1 0 1 --GREEN
toColor 6 = makeColor 0 0 1 1 --BLUE
toColor _ = makeColor 1 1 1 1 --White


--https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitOn'     :: (Char -> Bool) -> String -> [String]
splitOn' p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitOn' p s''
                            where (w, s'') = break p s'