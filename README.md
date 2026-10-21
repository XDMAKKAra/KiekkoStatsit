# KiekkoStatsit

<!-- badges: start -->
[![R-CMD-check](https://github.com/XDMAKKAra/KiekkoStatsit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/XDMAKKAra/KiekkoStatsit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Hockey data for R, with a focus on Finnish **Liiga**.

KiekkoStatsit pulls Liiga schedules, goals, shot locations, box scores,
standings and player stats straight from the liiga.fi API into tidy tibbles.
It also keeps the NHL play-by-play and hockey-reference.com scrapers from
[hockeyR](https://github.com/danmorse314/hockeyR), the package it is built on.

## Installation

```r
# install.packages("remotes")
remotes::install_github("XDMAKKAra/KiekkoStatsit")
```

## Liiga quick start

```r
library(KiekkoStatsit)
library(dplyr)

# Seasons are named by the year they end in: 2025 = 2024-25
games <- liiga_games(2025)
games %>% filter(ended) %>% select(game_id, home_team, away_team, home_goals, away_goals, result_type)

# Every goal of the season in one request
goals <- liiga_goals(2025)
goals %>% filter(power_play) %>% count(scorer_name, sort = TRUE)

# League table and skater stats
liiga_standings(2025)
liiga_skater_stats(2025) %>% select(player_name, team_name, games, goals, assists, points, expected_goals)

# One game in detail
game <- liiga_game(400, 2025)
game$rosters    # dressed players with line and position
game$penalties  # with penalty codes and who served bench minors
game$referees

# Shot attempts with location, strength and Liiga's xG
shots <- liiga_shots(400, 2025)
shots %>% count(team, shot_result)

# Full-game box score with tracking data
box <- liiga_boxscore(400, 2025)
box$skaters %>% select(player_name, time_on_ice, corsi_for, corsi_against, top_speed, hardest_shot)
box$goalies

# Goalie season stats, including goals saved above expected
liiga_goalie_stats(2025)
```

## Liiga functions

| Function | What you get |
|---|---|
| `liiga_games()` | Schedule and results: score, xG, power plays, attendance, arena |
| `liiga_goals()` | Every goal of a season: scorer, assists, score, goal type flags |
| `liiga_game()` | One game: info, lineups, goals with on-ice jerseys, penalties, referees |
| `liiga_game_rosters()` | Lineups for one or more games |
| `liiga_shots()` | Shot attempts with coordinates, distance, angle, strength and xG |
| `liiga_boxscore()` | Team, skater and goalie stats per game, incl. faceoffs by zone, corsi, passes, skating distance, top speed |
| `liiga_goalie_stats()` | Goalie season totals built from box scores: SV%, GAA, GSAx |
| `liiga_standings()` | League table (regular season, playoffs, playout, qualifications, preseason) |
| `liiga_skater_stats()` | Season totals for every skater |
| `liiga_player()` | Player bio and season-by-season Liiga history |
| `liiga_teams()` | Team ids, names, arena capacity, website, logo |
| `liiga_current_season()` | The season currently being played |

`tournament` takes `"runkosarja"` (regular season, default), `"playoffs"` or
`"valmistavat_ottelut"` (preseason). `"regular"` and `"preseason"` work too.

### Things worth knowing

- **Goal types.** Liiga's codes are kept in `goal_types` and the common ones are
  turned into flags: `YV`/`YV2` power play, `AV` short-handed, `TM` empty net,
  `IM` extra attacker, `SR` delayed penalty, `RL` penalty shot, `VL` shootout
  winner. `RL0` (missed penalty shot) and `VT0` (goal overturned on video review)
  are not goals and are dropped by default. With those dropped, the goals of
  every 2024-25 regular season game add up to the final score.
- **Shot coordinates.** `x_raw`/`y_raw` are what Liiga sends. `x`/`y` are metres
  from center ice with the shooting team always attacking towards positive `x`,
  assuming a 60 x 30 m rink. Treat `x`, `y`, `shot_distance` and `shot_angle`
  as approximate. Shot locations are missing for some games.
- **Goalies.** Liiga's API has no goalie stats table, so `liiga_goalie_stats()`
  fetches every box score of the season. That is two requests per game and takes
  a few minutes for a full regular season.
- **Unofficial API.** liiga.fi has no documented public API. The endpoints used
  here are the ones the liiga.fi website itself calls, and they can change
  without notice. If they move, `options(kiekkostatsit.liiga_url = "...")`
  changes the base URL.

## NHL and hockey-reference.com

Everything from hockeyR is still here:

- `load_pbp()` loads full NHL seasons of play-by-play (2010-11 onwards) from the
  [hockeyR-data](https://github.com/danmorse314/hockeyR-data) repository.
- `scrape_game()`, `scrape_day()`, `scrape_season()`, `get_game_ids()`,
  `get_game_info()`, `get_game_rosters()` and `get_game_shifts()` scrape the
  NHL API directly.
- `calculate_xg()`, `calculate_toi()`, `calculate_on_ice()` and
  `calculate_individual()` compute stats from play-by-play.
- `get_skater_stats_hr()`, `get_goalie_stats_hr()`, `get_team_records()`,
  `get_rosters()`, `get_jersey_players()` and `get_player_stats_hr()` scrape
  hockey-reference.com.

```r
pbp <- load_pbp("2023-24")
```

**Known broken:** `get_standings()`, `get_current_rosters()`,
`get_team_rosters()` and `get_draft_class()` still call the old
`statsapi.web.nhl.com` API, which the NHL shut down in 2023. They need to be
moved to `api-web.nhle.com` before they work again.

## Development

Tests run offline against trimmed real liiga.fi responses in
`tests/testthat/fixtures`, so they don't depend on the API being up.

```r
devtools::test()
```

## Credits and license

Built on [hockeyR](https://github.com/danmorse314/hockeyR) by Daniel Morse.
MIT licensed, see [LICENSE](LICENSE). Liiga data belongs to Liiga; this
package is not affiliated with Liiga or the NHL.
