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
initializeLevelState (fr,sp) = initialLevelState {fallingRegions = fr, speed = sp}

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
        Caller updatedGameState (getCall updatedGameState input)
    where
        p1         = player lvlState
        p2         = player2 lvlState
        jp2        = fromJust p2
        newRegions = updateRegionsTick (speed lvlState) secs (fallingRegions lvlState)
        newPlayerLocation  = case p1 of
            (Player _ _ _ False) -> movePlayer p1 (inputToMovement input) newRegions
            (Player _ _ _ True)  -> movePlayer p1 (movementAi lvlState p1) newRegions            
        newPlayer2Location = case p2 of
            Just p@(Player _ _ _ False)  -> movePlayer p (inputToMovement2 input) newRegions
            Just p@(Player _ _ _ True)   -> movePlayer p (movementAi lvlState jp2) newRegions

getCall :: LevelState -> InputState -> Maybe Call
getCall ls input
    | paused ls                                               = Nothing
    | keyEscPress input                                       = Just ShowMenu
    | elapsedTime ls >= 20 && (not (paused ls))               = Just (EndGame "YOU WON!")
    | isJust (player2 ls) && hit (fromJust (player2 ls)) && hit (player ls) = Just (EndGame "You both lost!")        
    | hit (player ls) && not (isJust (player2 ls))            = Just (EndGame "YOU LOST")
    | hit (player ls) && isJust (player2 ls)                  = Just (EndGame "Player 1 Won!")
    | isJust (player2 ls) && hit (fromJust (player2 ls))      = Just (EndGame "Player 2 Won!")    
    | hit (player ls) && not (isJust (player2 ls))            = Just (EndGame "YOU LOST")    
    | otherwise                                               = Nothing

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
                                Left int -> generateRandomLevel newRand
                        let initialLevel = initializeLevelState x
                            level = case playOptions options of
                                SinglePlayer -> initialLevel
                                MultiPlayer  -> initialLevel{player2 = Just initialPlayer}
                                Ai           -> initialLevel{player2 = Just (initialPlayer{ai = True})}
                                in return $ level
                        
        
-- Functions to modify the level

newRand = randomIO :: IO Int

generateRandomLevel :: IO Int -> IO (([FallingRegion]),Float)
generateRandomLevel s = do birdseed <- s
                           return ([  rndToFallingRegion(rndToIntList (birdseed+x)) | x <- [0..5] ],1)
    
rndToIntList :: Int -> [Int]
rndToIntList s = map (abs . (`mod`100)) (take 10 $ randomList s ) 
    
randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int] 

rndToFallingRegion :: [Int] -> FallingRegion
rndToFallingRegion xs = map rndToFallingShape xs  

rndToFallingShape :: Int -> FallingShape
rndToFallingShape x = FallingShape (fromIntegral x) 1 (toColor(x`mod`7))



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