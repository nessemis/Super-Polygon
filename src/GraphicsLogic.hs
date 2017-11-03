module GraphicsLogic (centerPicture, playerPicture, playerPosition,fallingRegionsPicture,scorePicture) where

import Model
import LevelModel

import Prelude

import Graphics.Gloss
import Data.Fixed
import System.Random

-------------------------------------------------------
--SCORE------------------------------------------------
-------------------------------------------------------
--Make it Boop for nice effect
scorePicture :: LevelState -> Picture
scorePicture ls = Translate 20 15   $ 
                 scale'   $
                 color'             $
                 Text $ show $ time
                    where time = (score ls)
                          color' = color (if  round < lnboop then red else yellow )
                          scale' = (if round <lnboop then scale (0.02 + 0.025*sinalboop) (0.02 + 0.025*sinalboop) else scale 0.02 0.02)
                          round  = mod' time booptime
                          sinalboop = sin ((((round) / lnboop )) *  pi)
                          lnboop    = 0.3
                          booptime = 5
                         
-------------------------------------------------------
--FALLINGREGIONS---------------------------------------
-------------------------------------------------------

fallingRegionsPicture :: LevelState -> Picture
fallingRegionsPicture ls = pictures [regionPicture t n (fallingRegions ls !! n)| n <- [0 .. (t - 1)] ]
        where t = length (fallingRegions ls)

-------------------------------------------------------
--CENTER POLYGON---------------------------------------
-------------------------------------------------------

centerPicture :: LevelState -> Picture
centerPicture ls = (rotate timeStep . centerColor) ls
    where 
        timeStep = elapsedTime ls * 20

centerColor :: LevelState -> Picture
centerColor = color red . centerEdges

centerEdges :: LevelState -> Picture
centerEdges = regularNPolygon . length . fallingRegions

-------------------------------------------------------
--THE PLAYER-------------------------------------------
-------------------------------------------------------
playerPicture :: LevelState -> Picture
playerPicture ls | (hit ls)  = rot ( trans ( playerColor ( playerDeathEdges) ls))
                 | otherwise = rot (trans (playerColor (playerEdges) ls))
        where total = fromIntegral $ length $ fallingRegions ls
              trans = translate 2.0 0.0 
              rot   = rotate (360.0 * (getPos(player ls)) / total)
              
              
playerColor :: (LevelState -> Picture) -> LevelState -> Picture
--playerColor f ls = if (keyEnter (inputState ls))  then ( color white . f) ls else ( color green . f) ls
playerColor f ls = (color white . f) ls


playerDeathEdges :: LevelState -> Picture
playerDeathEdges ls = pictures [ (rotate y $ scale (0.25 /z) (0.25/z) $ Translate ( x) (  x) $ (regularNPolygon 3)) 
                                                        | let x = (getAnimTime (player ls))/30,
                                                          let z =(x/10+1) ,
                                                          ey<-[1..12],
                                                          let y = (ey*30)]

                                                         
playerEdges :: LevelState -> Picture
playerEdges = const (regularNPolygon 3)

--Helper Function to get the player position
getPos :: Player -> Float
getPos (Player p a) = p
--Helper function to get the player animation time
getAnimTime :: Player -> Float
getAnimTime (Player _ aq) = aq

playerPosition :: LevelState -> Picture
playerPosition ls = Translate (2) (2) $ scale (0.02) (0.02) $ pictures [ Color blue $ Text $ show (getPos (player ls)),
                    Translate (-200) 0 $ Color yellow $ Text $ show $ fromIntegral $ length (fallingRegions ls)] 


--------------------------------------------------------
--REGION SHAPE------------------------------------------
--------------------------------------------------------

regionPicture :: Int -> Int -> FallingRegion -> Picture
regionPicture t n r = rotate (360 * (fromIntegral (n + 1)) / (fromIntegral t)) $ pictures (map (squarePicture t) r)

--------------------------------------------------------

squarePicture :: Int -> FallingShape -> Picture
squarePicture t (FallingShape d h) | (d < drawingDistance) = squareColor d h t
                                   | otherwise = Blank
                                   
squareColor :: Float -> Float -> Int -> Picture
squareColor d h t = color red (squareEdges d h t)

squareEdges :: Float -> Float -> Int -> Picture
squareEdges d h t = polygon [(d, 0), (d + h, 0), ((d + h) * cos(angle), (d + h) * sin(angle)), (d * cos(angle), d * sin(angle))]
        where angle = 2.0 * pi / (fromIntegral t)

-- ====================================================
-- POLYGON DRAWER--------------------------------------
-- ====================================================

regularNPolygon :: Int -> Picture
regularNPolygon t = polygon [(cos (x * stepSize), sin (x * stepSize)) | a <- [0 .. (t - 1)], let x = fromIntegral a]
    where stepSize = 2 * pi / (fromIntegral t)
