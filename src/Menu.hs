module Menu where
        
import MenuModel
import InputModel
import Model

updateMenuState :: InputState -> MenuState -> Caller MenuState
updateMenuState iState menuState@MenuState{screen = currentScreen}
      | not $ visible menuState = Caller menuState Nothing
      | otherwise = let updatedScreen = updateCurrentScreen iState currentScreen in
                  Caller menuState{screen = fst updatedScreen} $ snd updatedScreen


updateCurrentScreen :: InputState -> Screen -> (Screen, Maybe Call)
updateCurrentScreen (InputState{keyEscPress = True}) screen = (screen, Just ResumeGame)
updateCurrentScreen iState screen@LevelSelect{levels = levels, selectedButton = selectedButton}
      | keyRightPress iState = (screen{selectedButton = if (selectedButton + 1) < length levels + 1 then selectedButton + 1 else selectedButton}, Nothing)
      | keyLeftPress  iState = (screen{selectedButton = if (selectedButton - 1) >= 0                then selectedButton - 1 else selectedButton}, Nothing)        
      | keyEnterPress iState = if selectedButton == 0 then (LevelOptionsSelect (LevelParameters (Left selectedButton) SinglePlayer) screen, Nothing)
                                                      else (LevelOptionsSelect (LevelParameters (Right ("levels\\" ++ (levels !! (selectedButton - 1)) ++ ".txt" )) SinglePlayer ) screen, Nothing)
      | otherwise             = ((updateDisplacement screen), Nothing)

updateCurrentScreen iState screen@(EndGameMessage m levelSelectScreen) 
      | keyEnterPress iState = (levelSelectScreen, Nothing)
      | otherwise            = (screen, Nothing)

updateCurrentScreen iState screen@(LevelOptionsSelect levelParameters@LevelParameters{ levelOption = option} levelSelectScreen)
      | keyLeftPress iState  = (LevelOptionsSelect levelParameters{levelOption = cycleOptions (Left option)} levelSelectScreen, Nothing)
      | keyRightPress iState = (LevelOptionsSelect levelParameters{levelOption = cycleOptions (Right option)} levelSelectScreen, Nothing)
      | keyEnterPress iState = (levelSelectScreen, Just (StartLevel levelParameters))
      | otherwise            = (screen, Nothing)

cycleOptions :: Either LevelOptions LevelOptions -> LevelOptions
cycleOptions (Left option) = case option of
      SinglePlayer -> Ai
      MultiPlayer  -> SinglePlayer
      Ai           -> MultiPlayer
cycleOptions (Right option) = case option of
      SinglePlayer -> MultiPlayer
      MultiPlayer  -> Ai
      Ai           -> SinglePlayer
      
                                                 
updateDisplacement :: Screen -> Screen
updateDisplacement ms = ms{displacement = dis + (if abs(dif) <= 0.01 then 0 else dif / abs(dif) ) / 25}
                                    where sel' = fromIntegral $ selectedButton ms
                                          dis  = displacement ms
                                          dif  = sel' - dis