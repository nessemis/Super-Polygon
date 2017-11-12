module GraphicsModel where

data LevelAnimation = LevelAnimation{
    rotationDirection :: Int,
    verticalExpansion :: Int,
    verticalExpansionTarget :: Int,
    horizontalExpension :: Int,
    horizontalExpensionTarget :: Int,
    animationTime     :: Int,
    animationTimeTarget :: Int,
    randomshit    
}