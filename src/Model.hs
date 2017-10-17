-- | This module contains the data types
--   which represent the state of the game
module Model where

--import GraphicsModel

import System.IO
import Control.Monad
import Control.Monad.IO.Class

fallspeed :: Float
fallspeed = 1

drawingDistance :: Float
drawingDistance = 20

type Player          = Float                  --(Region in terms of float).
data  FallingShape   = FallingShape Float Float --(Distance bottom to floor) Height 
type FallingRegion   = [FallingShape]

data InputState = InputState {
  keyLeft  :: Bool,
  keyRight :: Bool
}

data GameState = GameState {
                   menu           :: Bool
                 , hit            :: Bool
                 , player         :: Player
                 , fallingRegions :: [FallingRegion]
                 , inputState     :: InputState
                 , elapsedTime    :: Float
                 }


                 
initialState :: [FallingRegion] -> GameState
initialState fr = GameState False False 0 fr (InputState False False) 0
--initialState = GameState False False 0 [[FallingShape 3 1],[FallingShape 4 1], [FallingShape 5 2], [FallingShape 6 1]] (InputState False False) 0


readLevelFile :: FilePath -> IO [FallingRegion]
readLevelFile f = do
            fileContent <-  readFile f
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