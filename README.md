# `starlight`

A game.


## Development

### Prerequisites

Development currently assumes a Mac with `ghc` 8.8 & `cabal` 3.0. You can install them directly or use [`ghcup`](https://www.haskell.org/ghcup/).

Once those are installed, the [`sdl2` package](https://github.com/haskell-game/sdl2) requires the SDL library to be installed. You can find instructions on how to do this in the `sdl2` package README.

### Execution

Run `script/repl` to load the project (both library & executable) into the REPL (you might need to do a `cabal build` first). In the REPL, `:main` will launch the game.
