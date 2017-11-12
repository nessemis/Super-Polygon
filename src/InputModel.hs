module InputModel where
        
data InputState = InputState {
    keyPause :: Bool
    , keyLeft       :: Bool
    , keyRight      :: Bool
    , keyA          :: Bool
    , keyD          :: Bool
    , keyPausePress :: Bool    
    , keyLeftPress  :: Bool
    , keyRightPress :: Bool
    , keyEscPress   :: Bool
    , keyEnterPress :: Bool
    , keyQPress     :: Bool
}

initialInputState :: InputState
initialInputState = InputState False False False False False False False False False False False