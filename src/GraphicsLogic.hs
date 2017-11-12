module GraphicsLogic (centerPicture, playerPictures,fallingRegionsPicture,scorePicture,menuPicture) where

import Model
import LevelModel
import MenuModel

import Prelude

import Graphics.Gloss
import Data.Fixed
import System.Random


-------------------------------------------------------
--MENU-------------------------------------------------
-------------------------------------------------------

--Draw the menu
menuPicture :: MenuState -> Picture
menuPicture (MenuState False _) = Blank
menuPicture (MenuState True (s@LevelSelect{displacement = displacement, levels = levelStrings})) = Translate (displacement * (-50)) 0 $ 
                                                                    pictures [menuButton buttonString (fromIntegral (50 * n)) | n <- [0..length allLevelStrings - 1], let buttonString = allLevelStrings !! n]
                                                                    where allLevelStrings = "RANDOM" : levelStrings
menuPicture (MenuState True s@(EndGameMessage m _)) = menuSplash m 
menuPicture (MenuState True s@(LevelOptionsSelect options _)) = menuButton (show (levelOption options)) 0

         
--Draw a nice menu button with a text
menuButton :: String -> Float -> Picture
menuButton name pos =  Translate pos 0 $ scale 20 20 $ pictures [
                                            rotate 45 $ Color (makeColor 0.6 0.6 0.6 1) $ regularNPolygon 4,
                                            rotate 45 $ color blue$ regularNLine 4,
                                            Translate ((-0.025)*fromIntegral(length name)) 0 $ scale 0.001 0.001 $ color blue  $ Text name
                                            ]
                                            
--Draw a splash screen for winning or losing
menuSplash :: String -> Picture
menuSplash name = Translate (0) (10) $ scale 20 5 $ pictures [
                                            rotate 45 $ Color (makeColor 0.2 0.2 0.2 1) $ regularNPolygon 4,
                                            rotate 45 $ color blue$ regularNLine 4,
                                            Translate ((-0.04)*fromIntegral(length name)) 0 $ scale 0.001 0.004 $ color red  $ Text name
                                            ]

-------------------------------------------------------
--SCORE------------------------------------------------
-------------------------------------------------------
--Display the current score and make it Boop for nice effect
scorePicture :: LevelState -> Picture
scorePicture ls = Translate 20 15   $ 
                  scale'            $
                  color'            $
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
--Draw the levels falling regions
fallingRegionsPicture :: LevelState -> Picture
fallingRegionsPicture ls = pictures [regionPicture t n (fallingRegions ls !! n)| n <- [0 .. (t - 1)] ]
        where t     = length (fallingRegions ls)
             
--The individual pieces of one region
regionPicture ::  Int -> Int -> FallingRegion -> Picture
regionPicture t n r = rotate (360 * (fromIntegral (n + 1)) / (fromIntegral t)) $ pictures (map (squarePicture  t) r)

squarePicture :: Int -> FallingShape -> Picture
squarePicture  t (FallingShape d h c) | (d < drawingDistance) = Color c $ squareEdges  d h t
                                         | otherwise = Blank
                                   

squareEdges :: Float -> Float -> Int -> Picture
squareEdges d h t = polygon [(d, 0), (d + h, 0), ((d + h) * cos(angle), (d + h) * sin(angle)), (d * cos(angle), d * sin(angle))]
        where angle = 2.0 * pi / (fromIntegral t)
        
-------------------------------------------------------
--CENTER POLYGON---------------------------------------
-------------------------------------------------------
--The center picture spins for nice effect
centerPicture :: LevelState -> Picture
centerPicture ls = (rotate timeStep . centerColor) ls
    where 
        timeStep = elapsedTime ls * 20

centerColor :: LevelState -> Picture
centerColor = color red . centerEdges

centerEdges :: LevelState -> Picture
centerEdges = regularNPolygon . length . fallingRegions


-------------------------------------------------------
--THE PLAYERS------------------------------------------
-------------------------------------------------------

playerPictures :: Player -> Maybe Player -> Int -> Picture
playerPictures p1 p2 r = case p2 of
    Nothing  -> p1Picture
    Just jp2 -> let p2Picture = playerPicture jp2 ( blue) r in
                pictures [p1Picture, p2Picture]
    where p1Picture = playerPicture p1 ( white) r
          

--Draw one player
playerPicture :: Player -> Color -> Int -> Picture --The int is the amount of fallingregions
playerPicture p c r | (hit p)  = rot ( trans ( playerColor ( playerDeathEdges) c p))
                        | otherwise = rot (trans (playerColor (playerEdges) c p))
        where total = fromIntegral $ r
              trans = translate 2.0 0.0 
              rot   = rotate (360.0 * (getPos p) / total)
              
              
playerColor :: (Player -> Picture) -> Color-> Player -> Picture
playerColor f c p = ((color c) . f) p

playerDeathEdges :: Player -> Picture
playerDeathEdges p = pictures [ (rotate y $ scale (0.25 /z) (0.25/z) $ Translate ( x) (  x) $ (regularNPolygon 3)) 
                                                        | let x = (getAnimTime p)/30,
                                                          let z =(x/10+1) ,
                                                          ey<-[1..12],
                                                          let y = (ey*30)]

                                                         
playerEdges :: Player -> Picture
playerEdges = const (regularNPolygon 3)

--Helper Function to get the player position
getPos :: Player -> Float
getPos p = location p
--Helper function to get the player animation time
getAnimTime :: Player -> Float
getAnimTime p = deathAnimation p


-- ====================================================
-- POLYGON DRAWER--------------------------------------
-- ====================================================

regularNPolygon :: Int -> Picture
regularNPolygon t = polygon [(cos (x * stepSize), sin (x * stepSize)) | a <- [0 .. (t - 1)], let x = fromIntegral a]
    where stepSize = 2 * pi / (fromIntegral t)
    
regularNLine :: Int -> Picture
regularNLine t = line [(cos (x * stepSize), sin (x * stepSize)) | a <- [0 .. (t)], let x = fromIntegral a]
    where stepSize = 2 * pi / (fromIntegral t)
