module GraphicsLogic (centerPicture, playerPicture, playerPosition,fallingRegionsPicture) where

import Model
import Prelude

import Graphics.Gloss

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
playerPicture gs =  rotate (360.0 * (player gs) / total) (translate 2.0 0.0 (playerColor gs))
        where total = fromIntegral $ length $ fallingRegions gs

playerColor :: GameState -> Picture
playerColor = color white . playerEdges

playerEdges :: GameState -> Picture
playerEdges = rotate 90.0 . const (regularNPolygon 3)

playerPosition :: GameState -> Picture
playerPosition gs = Translate (2) (2) $ scale (0.02) (0.02) $ pictures [ Color blue $ Text $ show (player gs),
                    Translate (-200) 0 $ Color yellow $ Text $ show $ fromIntegral $ length (fallingRegions gs)] 

--------------------------------------------------------
--REGION SHAPE------------------------------------------
--------------------------------------------------------

regionPicture :: Int -> Int -> FallingRegion -> Picture
regionPicture t n r = rotate (360 * (fromIntegral n) / (fromIntegral t)) $ pictures (map (squarePicture t) r)

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
regularNPolygon t = polygon [(sin (x * stepSize), cos (x * stepSize)) | a <- [0 .. (t - 1)], let x = fromIntegral a]
    where stepSize = 2 * pi / (fromIntegral t)
