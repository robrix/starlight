# `starlight`

A game.


## Development

Development currently assumes a Mac with `ghc` 8.8 & `cabal` 3.0. You can install them directly, or use [`ghcup`](https://www.haskell.org/ghcup/).

Initial setup:

```bash
brew bundle
cabal build
```

Run `script/repl` to load the project (both library & executable) into the REPL. In the REPL, `:main` will launch the game.
