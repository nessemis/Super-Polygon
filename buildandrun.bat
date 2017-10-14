cabal sandbox init
cabal update # Obtain package information
cabal install --only-dependencies
cabal build
cabal run your-project