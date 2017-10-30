module GraphicsLogic (centerPicture, playerPicture, playerPosition,fallingRegionsPicture,scorePicture) where

import Model
import Prelude

import Graphics.Gloss
import Data.Fixed
import System.Random

-------------------------------------------------------
--SCORE------------------------------------------------
-------------------------------------------------------
--Make it Boop for nice effect
scorePicture :: GameState -> Picture
scorePicture gs = Translate 20 15   $ 
                 scale'   $
                 color'             $
                 Text $ show $ time
                    where time = (score gs)
                          color' = color (if  round < lnboop then red else yellow )
                          scale' = (if round <lnboop then scale (0.02 + 0.025*sinalboop) (0.02 + 0.025*sinalboop) else scale 0.02 0.02)
                          round  = mod' time booptime
                          sinalboop = sin ((((round) / lnboop )) *  pi)
                          lnboop    = 0.3
                          booptime = 5
                         
-------------------------------------------------------
--FALLINGREGIONS---------------------------------------
-------------------------------------------------------

fallingRegionsPicture :: GameState -> Picture
fallingRegionsPicture gs = pictures [regionPicture t n (fallingRegions gs !! n)| n <- [0 .. (t - 1)] ]
        where t = length (fallingRegions gs)

-------------------------------------------------------
--CENTER POLYGON---------------------------------------
-------------------------------------------------------

centerPicture :: GameState -> Picture
centerPicture gs = (rotate timeStep . centerColor) gs
    where 
        timeStep = elapsedTime gs * 20

centerColor :: GameState -> Picture
centerColor = color red . centerEdges

centerEdges :: GameState -> Picture
centerEdges = regularNPolygon . length . fallingRegions

-------------------------------------------------------
--THE PLAYER-------------------------------------------
-------------------------------------------------------
playerPicture :: GameState -> Picture
playerPicture gs | (hit gs)  = ( trans ( playerColor ( playerDeathEdges) gs))
                 | otherwise = rot (trans (playerColor (playerEdges) gs))
        where total = fromIntegral $ length $ fallingRegions gs
              trans = translate 2.0 0.0 
              rot   = rotate (360.0 * (player gs) / total)
              
              

              
playerColor :: (GameState -> Picture) -> GameState -> Picture
playerColor f gs = (color white . f) gs

playerDeathEdges :: GameState -> Picture
playerDeathEdges gs = pictures [ (rotate y $ scale (0.25) (0.25) $ Translate ( x) (  x) $ (regularNPolygon 3)) 
                                                        | ex<-[1..4] ,
                                                          let x = (ex + sin(elapsedTime gs)*10),
                                                          ey<-[1..4],
                                                          let y = (ey*75)]
                                                         
playerEdges :: GameState -> Picture
playerEdges = const (regularNPolygon 3)

playerPosition :: GameState -> Picture
playerPosition gs = Translate (2) (2) $ scale (0.02) (0.02) $ pictures [ Color blue $ Text $ show (player gs),
                    Translate (-200) 0 $ Color yellow $ Text $ show $ fromIntegral $ length (fallingRegions gs)] 

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
