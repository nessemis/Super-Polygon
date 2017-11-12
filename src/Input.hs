module Input where

import InputModel
import Graphics.Gloss.Interface.IO.Game
import Model

--Nullifies any presses. Basically only allowing for a key to be pressed one frame
updateInputState :: InputState -> Caller InputState
updateInputState inputState = 
    let 
        call = if keyQPress inputState then Just QuitGame else Nothing
        updatedInputState = inputState {
            keyLeftPress  = False,
            keyRightPress = False,
            keyPausePress = False,
            keyEscPress   = False,
            keyEnterPress = False
        }
        in Caller updatedInputState call

inputKey :: Event -> InputState -> InputState
inputKey (EventKey (SpecialKey c) d _ _) istate
  = case c of
      KeyLeft  -> istate {keyLeft       = down,
                          keyLeftPress  = down}
      KeyRight -> istate {keyRight      = down,
                          keyRightPress = down}
      KeyEsc   -> istate {keyEscPress   = down}
      KeyEnter -> istate {keyEnterPress = down}
      _        -> istate
  where down = d == Down
      

--Check for normal keys
inputKey (EventKey (Char c) d _ _) istate
   = case c of
        'a' -> istate {keyA            = down}
        'd' -> istate {keyD            = down}
        'p' -> istate {keyPause        = down,
                       keyPausePress   = down}
        'q' -> istate {keyQPress       = down}                       
        _   -> istate 
    where down = d == Down
        
inputKey _ istate = istate -- Otherwise keep the same