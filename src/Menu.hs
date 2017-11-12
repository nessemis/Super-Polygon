module Menu where
        
import MenuModel
import InputModel
import Model

updateMenuState :: InputState -> MenuState -> Caller MenuState
updateMenuState is menuState
      | not $ visible menuState = Caller menuState Nothing
      | otherwise = let currentScreen = screen menuState
                        updatedScreen = updateCurrentScreen is currentScreen in
                  Caller menuState{screen = fst updatedScreen} (snd updatedScreen)


updateCurrentScreen :: InputState -> Screen -> (Screen, Maybe Call)
updateCurrentScreen (InputState{keyEscPress = True}) s   = (s, Just ResumeGame)
updateCurrentScreen is (s@LevelSelect{}) 
      | keyRightPress is = (s{selectedButton = if ((sel + 1) < 3) then sel + 1 else sel}, Nothing)
      | keyLeftPress  is = (s{selectedButton = if ((sel - 1) >= 0) then sel - 1 else sel}, Nothing)        
      | keyEnterPress is = (LevelOptionsSelect (LevelOptions (Right ("lvl" ++ (show sel) ++ ".txt" )) SinglePlayer), Nothing)
      | otherwise        = ((updateDisplacement s), Nothing)
      where sel = selectedButton s
updateCurrentScreen is s@(EndGameMessage m) 
      | keyEnterPress is = (initialScreen, Nothing)
      | otherwise        = (s, Nothing)
updateCurrentScreen is s@(LevelOptionsSelect levelOptions)
      | keyLeftPress is  = (LevelOptionsSelect levelOptions{playOptions = cycleOptions (Left options)}, Nothing)
      | keyRightPress is = (LevelOptionsSelect levelOptions{playOptions = cycleOptions (Right options)}, Nothing)
      | keyEnterPress is = (initialScreen, Just (StartLevel levelOptions))
      | otherwise        = (s, Nothing)
      where options = playOptions levelOptions
      
cycleOptions :: Either PlayOptions PlayOptions -> PlayOptions
cycleOptions (Left option) = case option of
      SinglePlayer -> Ai
      MultiPlayer  -> SinglePlayer
      Ai           -> MultiPlayer
cycleOptions (Right option) = case option of
      SinglePlayer -> MultiPlayer
      MultiPlayer  -> Ai
      Ai           -> SinglePlayer
      
                                                 
updateDisplacement :: Screen -> Screen
updateDisplacement ms = ms{displacement = dis + (if abs(dif) <= 0.01 then 0 else (dif / abs(dif) ) / 25)}
                                    where sel' = fromIntegral (selectedButton ms)
                                          dis  = displacement ms
                                          dif  = (sel' - dis)