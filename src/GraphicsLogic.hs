module GraphicsLogic (centerPicture, playerPicture,fallingRegionsPicture) where

import Model
import Prelude

import Graphics.Gloss

-------------------------------------------------------
--FALLINGREGIONS---------------------------------------
-------------------------------------------------------

fallingRegionsPicture :: GameState -> Picture
fallingRegionsPicture gs = fallingShapePicture (head $ head (fallingRegions gs) )

fallingShapePicture (Square x y) = translate x y $ rotate 45 $ color blue $ regularNPolygon 4


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

regionPicture :: Int -> Int -> FallingRegion -> Picture
regionPicture t n r = pictures (map (squarePicture n) )

--------------------------------------------------------

squarePicture :: Int -> Float -> Float -> Picture
squarePicture n d h = squareColor d h n

squareColor :: Float -> Float -> Int -> Picture
squareColor d h n = color red (squareEdges d h n)

squareEdges :: Float -> Float -> Int -> Picture
squareEdges d h n = polygon [(d, 0), (d + h, 0), ((d + h) * sin(angle), (d + h) * cos(angle)), (d * sin(angle), d * cos(angle))]
        where angle = 2.0 * pi / (fromIntegral n)

regularNPolygon :: Int -> Picture
regularNPolygon n = polygon [(sin (x * stepSize), cos (x * stepSize)) | a <- [0 .. (n - 1)], let x = fromIntegral a]
    where stepSize = 2 * pi / (fromIntegral n)
