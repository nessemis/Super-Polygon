module GraphicsLogic (centerPicture, playerPicture,fallingRegionsPicture) where

import Model
import Prelude

import Graphics.Gloss

-------------------------------------------------------
--FALLINGREGIONS---------------------------------------
-------------------------------------------------------

--Not working
fallingRegionsPicture :: GameState -> Picture
fallingRegionsPicture gs = pictures [regionPicture t n (fallingRegions gs !! n)| n <- [0 .. (t - 1)]]
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
playerPicture gs = rotate (360.0 * (player gs) / total) (translate 2.0 0.0 (playerColor gs))
        where total = fromIntegral $ length $ fallingRegions gs

playerColor :: GameState -> Picture
playerColor = color white . playerEdges

playerEdges :: GameState -> Picture
playerEdges = rotate 90.0 . const (regularNPolygon 3)

-- ====================================================
-- POLYGON DRAWER--------------------------------------
-- ====================================================

--Not working
regionPicture :: Int -> Int -> FallingRegion -> Picture
regionPicture t n r = rotate (360 * (fromIntegral n) / (fromIntegral t)) $ pictures (map (squarePicture t) r)

--------------------------------------------------------

squarePicture :: Int -> FallingShape -> Picture
squarePicture t (FallingShape d h) = squareColor d h t

squareColor :: Float -> Float -> Int -> Picture
squareColor d h t = color red (squareEdges d h t)

squareEdges :: Float -> Float -> Int -> Picture
squareEdges d h t = polygon [(d, 0), (d + h, 0), ((d + h) * cos(angle), (d + h) * sin(angle)), (d * cos(angle), d * sin(angle))]
        where angle = 2.0 * pi / (fromIntegral t)

--------------------------------------------------------

regularNPolygon :: Int -> Picture
regularNPolygon t = polygon [(sin (x * stepSize), cos (x * stepSize)) | a <- [0 .. (t - 1)], let x = fromIntegral a]
    where stepSize = 2 * pi / (fromIntegral t)
