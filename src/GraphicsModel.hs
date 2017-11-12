module GraphicsModel where

import System.Random
import Data.Fixed

data AnimationState = AnimationState{
    rotationDirection         :: Float,
    rotationDirectionTarget   :: Float,
    verticalExpansion         :: Float,
    verticalExpansionTarget   :: Float,
    horizontalExpansion       :: Float,
    horizontalExpansionTarget :: Float,
    animationTime             :: Float,
    animationTimeTarget       :: Float,
    seed                      :: Int
}

updateLevelAnimation :: Float -> AnimationState -> AnimationState
updateLevelAnimation secs lAnimation@(AnimationState _ rotationDirectionTarget _ verticalExpansionTarget _ horizontalExpansionTarget animationTime animationTimeTarget seed)
    = if updatedAnimationTime <= animationTimeTarget then lAnimation{animationTime = updatedAnimationTime} else newAnimationState seed rotationDirectionTarget verticalExpansionTarget horizontalExpansionTarget
        where updatedAnimationTime = animationTime + secs

randomList :: Int -> [Float]
randomList seed = randoms (mkStdGen seed) :: [Float] 

range :: Float -> Float -> Float -> Float
range x lower upper = (abs (x `mod'` (upper - lower))) + lower

initialAnimationState :: Int -> AnimationState
initialAnimationState seed = newAnimationState seed 0 1 1

newAnimationState :: Int -> Float -> Float -> Float -> AnimationState
newAnimationState seed x y z = AnimationState  x
                                               (range (ls !! 0) (-100) 100)
                                               y
                                               (range (ls !! 1) 0.8 1.2)
                                               z
                                               (range (ls !! 2) 0.8 1.2)
                                               0
                                               (range (ls !! 3) 0.5 2)
                                               (seed + 100)
    where ls = take 4 $ randomList seed