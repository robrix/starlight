# `starlight`

A game.


## Development

Development currently assumes a Mac with `ghc` 8.8 & `cabal` 3.0. You can install them directly, or use [`ghcup`](https://www.haskell.org/ghcup/).

Initial setup:

```bash
brew bundle # for sdl2
cabal build # to set up dist-newstyle with the ghc package db
```

Run `script/repl` to load the project (both library & executable) into the REPL. In the REPL, `:main` will launch the game.

Alternatively, `cabal run exe:starlight` will launch the game.


## Controls

Controls are currently hard-coded; I intend to eventually make them configurable.

Up arrow: forward thrust
Left/right arrows: turn left/right
Down arrow: turn to face opposite direction from current heading
+/-: increase/decrease throttle (controls rate of thrust)
tab: switch to the next target
