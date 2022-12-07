# poker-sim

Odds calculator for poker. Uses a ranker that maps all possible hands to a score (see `ranker.clj`). In `core.clj`, you can specify the community cards, as well as current player hands, then call `simulate` to randomly generate a bunch of games and collect the overall game statistics.

## Usage

See `-main` in `core.clj` for an example.