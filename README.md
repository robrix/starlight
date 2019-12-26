# `starlight`

A game.


## Development

Development currently assumes a Mac with `ghc` 8.8 & `cabal` 3.0.

Initial setup:

```bash
brew bundle
cabal build
```

Run `script/repl` to load the project (both library & executable) into the REPL. In the REPL, `:main` will launch the game.
