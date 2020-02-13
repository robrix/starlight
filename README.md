# `starlight`

<img width="2048" alt="screenshot showing player’s ship in quite a close approach to Mercury" src="https://user-images.githubusercontent.com/59671/74091980-28e20780-4a8c-11ea-8ed8-15247cb06eaf.png">


## Development

Development currently assumes a Mac with `ghc` 8.8 & `cabal` 3.0. You can install them directly, or use [`ghcup`](https://www.haskell.org/ghcup/).

Initial setup:

```bash
brew bundle # for sdl2 & sqlite3
cat data/schema.sql | sqlite3 data/data.db # to create the solar system db
cat data/ephemerides.sql | sqlite3 data/data.db # to populate the solar system db with planets
cat data/factions.sql | sqlite3 data/data.db # to populate the solar system db with factions
cabal build # to set up dist-newstyle with the ghc package db
```

Run `script/repl` to load the project (both library & executable) into the REPL. In the REPL, `:main` will launch the game. Use `:main --profile` to enable profiling (timings for various parts of the game, shown on exit).

Alternatively, `cabal run starlight` will launch the game. Use `cabal run starlight -- --profile` to enable profiling.


## Controls

Controls are currently hard-coded; I intend to eventually make them configurable.

- Up arrow: forward thrust
- Left/right arrows: turn left/right
- Down arrow: turn to face opposite direction from current heading (relative to target’s heading, if any, or absolute otherwise; helps you match speed and heading to target’s)
- +/-: increase/decrease throttle (controls rate of thrust)
- tab/shift tab: switch to the next/prev target
- escape: clear the target
- space: fire weapons in the direction you’re facing
- t: turn to face the selected target (if any)
- f: face in the direction the ship is moving
- b: brake/match speed to target
- j: jump to the target (if any)
