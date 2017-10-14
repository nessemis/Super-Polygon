set extra-args=%1

IF extra-args == "update" (
cabal sandbox init
cabal update
cabal install
)


cabal build
cabal run