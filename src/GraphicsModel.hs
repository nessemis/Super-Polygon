module GraphicsModel where

import System.Random
import Data.Fixed

data LevelAnimation = LevelAnimation{
    rotationDirection         :: Float,
    verticalExpansion         :: Float,
    verticalExpansionTarget   :: Float,
    horizontalExpension       :: Float,
    horizontalExpensionTarget :: Float,
    animationTime             :: Float,
    animationTimeTarget       :: Float
}



randomList :: Int -> [Float]
randomList seed = randoms (mkStdGen seed) :: [Float] 




range :: Float -> Float -> Float -> Float
range x lower upper = (x `mod'` (upper - lower))+ lower

initialAnimationState seed = LevelAnimation (range (ls !! 0) 0 720)
                                            0
                                            (range (ls !! 1) 0.8 1.2)
                                            1
                                            (range (ls !! 2) 0.8 1.2)
                                            0
                                            (range (ls !! 3) 0.5 2)
                        where ls = take 4 $ randomList seed