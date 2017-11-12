module Level where
    
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import GameLogic

import System.Random

import LevelModel
import MenuModel
import InputModel
import Model

import Graphics.Gloss.Data.Color
    
initializeLevelState :: ([FallingRegion],Float) -> LevelState
initializeLevelState (fallingRegions, speed) = initialLevelState { fallingRegions = fallingRegions, speed = speed }

updateLevelState :: Float -> InputState -> LevelState -> Caller LevelState
updateLevelState secs input lState@(LevelState paused p1 p2 fallingRegions elapsedTime score speed) =
    let updatedLState
            | hit p1 = lState {
                player         = updateDeathPlayer p1
            }
            | isJust p2 && hit jp2 = lState {
                player2        = Just $ updateDeathPlayer jp2
            }
            | paused = lState{
                player         = updateDeathPlayer p1,
                paused         = not $ keyPausePress input         
            }
            | otherwise        = lState {
                paused         = keyPausePress input, 
                player         = p1 { hit = hit p1 || isHit p1 updatedFallingRegions,
                                      location = newPlayerLocation},
                fallingRegions = updatedFallingRegions,
                elapsedTime    = elapsedTime + secs ,
                score          = score + secs,
                player2        = if isJust p2 then Just jp2{ hit = hit jp2 || isHit jp2 updatedFallingRegions, location = newPlayer2Location} else p2
            }
    in
        Caller updatedLState $ getCall updatedLState input
    where
        jp2        = fromJust p2
        updatedFallingRegions = updateRegionsTick speed secs fallingRegions
        newPlayerLocation  = case p1 of
            (Player _ _ _ False) -> movePlayer p1 (inputToMovement input) updatedFallingRegions
            (Player _ _ _ True)  -> movePlayer p1 (movementAi lState p1) updatedFallingRegions            
        newPlayer2Location = case p2 of
            Just p@(Player _ _ _ False)  -> movePlayer p (inputToMovementP2 input) updatedFallingRegions
            Just p@(Player _ _ _ True)   -> movePlayer p (movementAi lState jp2) updatedFallingRegions

isGameFinished :: LevelState -> Bool
isGameFinished = isJust . getEndGameMessage

getCall :: LevelState -> InputState -> Maybe Call
getCall lState@(LevelState paused p1 p2 _ elapsedTime _ _) iState
    | paused                                   = Nothing
    | isJust endGameMessage                    = Just $ EndGame (fromJust endGameMessage)           
    | keyEscPress iState                       = Just ShowMenu
    | otherwise                                = Nothing
    where endGameMessage = getEndGameMessage lState

getEndGameMessage :: LevelState -> Maybe String
getEndGameMessage (LevelState paused p1 p2 _ elapsedTime _ _)
    | elapsedTime >= 20 && not paused          = Just "YOU WON!"
    | isJust p2 && hit (fromJust p2) && hit p1 = Just "You both lost!"      
    | hit p1    && not (isJust p2)             = Just "YOU LOST"
    | hit p1    && isJust p2                   = Just "Player 2 Won!"
    | isJust p2 && hit (fromJust p2)           = Just "Player 1 Won!" 
    | hit p1    && not (isJust p2)             = Just "YOU LOST"
    | otherwise                                = Nothing


inputToMovement :: InputState -> PlayerMovement
inputToMovement lState 
                | keyLeft lState  = MoveLeft
                | keyRight lState = MoveRight
                | otherwise       = Idle

inputToMovementP2 :: InputState -> PlayerMovement
inputToMovementP2 lState 
                | keyA lState = MoveLeft
                | keyD lState = MoveRight
                | otherwise   = Idle

movementAi :: LevelState -> Player -> PlayerMovement
movementAi LevelState{fallingRegions = fallingRegions} player = closestRegionLocation player fallingRegions

closestRegionLocation :: Player -> [FallingRegion] -> PlayerMovement
closestRegionLocation Player{location = location} fallingRegions 
    | abs direction <= 0.2  = Idle
    | direction >= 0        = MoveRight
    | direction <= 0        = MoveLeft
    where preferredLocation = 0.5 + (fromIntegral $ (fst . head . findHighestFallingShape) $ zip [0..] $ map closestShape fallingRegions)
          halfRegions       = (fromIntegral $ length fallingRegions) / 2
          distance          = preferredLocation - location
          direction         = if abs distance >= halfRegions then (abs distance) / distance * (-2) * halfRegions + distance else distance
          
closestShape :: [FallingShape] -> FallingShape
closestShape fallingRegion  
    | length fallingRegion == 0        = FallingShape 100 1 black --Just pretent the shape is really far away
    | extractHeight (head fallingRegion) <= 3 = closestShape (tail fallingRegion)
    | otherwise                        = head fallingRegion

findHighestFallingShape :: [(Int, FallingShape)] -> [(Int, FallingShape)] --Initially, we try to find the falling sh
findHighestFallingShape [x]    = [x]
findHighestFallingShape (x:xs) = let y = head xs in
            if (extractHeight . snd) x > (extractHeight . snd) y
                then findHighestFallingShape $ x:(tail xs) else findHighestFallingShape xs

startLevel :: LevelParameters -> IO LevelState
startLevel levelParameters = do
                        x <- case (randomOrLoad levelParameters) of
                                Right path -> readLevelFile path
                                Left int -> generateRandomLevel newRand
                        let initialLevel = initializeLevelState x
                            level = case levelOption levelParameters of
                                SinglePlayer -> initialLevel
                                MultiPlayer  -> initialLevel{player2 = Just initialPlayer}
                                Ai           -> initialLevel{player2 = Just (initialPlayer{ai = True})}
                                in return $ level
                        
        
-- Functions to modify the level

newRand = randomIO :: IO Int

generateRandomLevel :: IO Int -> IO (([FallingRegion]),Float)
generateRandomLevel s = do birdseed <- s
                           return ([  rndToFallingRegion(rndToIntList (birdseed+x)) | x <- [0..4] ],fromIntegral(birdseed`mod`3 + 2))
    
rndToIntList :: Int -> [Int]
rndToIntList s = map (abs . (`mod`100)) (take 10 $ randomList s ) 
    
randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int] 

rndToFallingRegion :: [Int] -> FallingRegion
rndToFallingRegion xs = map rndToFallingShape xs  

rndToFallingShape :: Int -> FallingShape
rndToFallingShape x = FallingShape (fromIntegral (x + 5)) 1 (toColor(x`mod`7))



-- IMPURE

readLevelFile :: FilePath -> IO ([FallingRegion],Float)
readLevelFile f = do
            fileContent <- readFile f
            return ((map lineToFallingRegion (tail(lines fileContent))), read $ head $ (lines fileContent) )

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