test_that("liiga_current_season switches in July", {
  expect_equal(liiga_current_season(as.Date("2025-06-30")), 2025L)
  expect_equal(liiga_current_season(as.Date("2025-07-01")), 2026L)
  expect_equal(liiga_current_season(as.Date("2026-01-15")), 2026L)
})

test_that("tournament aliases resolve and bad names error", {
  expect_equal(liiga_tournament("regular"), "runkosarja")
  expect_equal(liiga_tournament("Playoffs"), "playoffs")
  expect_equal(liiga_tournament("preseason"), "valmistavat_ottelut")
  expect_error(liiga_tournament("allstar"), "Unknown tournament")
})

test_that("to_snake converts api names", {
  expect_equal(
    to_snake(c("plusMinus", "timeOnIceAvg", "shotX", "xge", "team_logos.darkBg")),
    c("plus_minus", "time_on_ice_avg", "shot_x", "xge", "team_logos_dark_bg")
  )
})

test_that("liiga_games parses results", {
  local_mocked_bindings(liiga_get = mock_liiga)

  games <- liiga_games(2025)

  expect_equal(games$game_id, c(1, 3, 5, 24))
  expect_setequal(games$result_type, c("REG", "SO", "OT"))
  expect_s3_class(games$start_time, "POSIXct")
  expect_false(anyNA(games$home_team))
  expect_true(all(games$home_team_id > 0))
})

test_that("unplayed games keep NA results", {
  game <- parse_liiga_game_row(read_fixture("games_future")[[1]])

  expect_false(game$ended)
  expect_true(is.na(game$result_type))
})

test_that("liiga_goals adds up to the final scores", {
  local_mocked_bindings(liiga_get = mock_liiga)

  games <- liiga_games(2025)
  goals <- liiga_goals(2025)

  per_game <- table(factor(goals$game_id, levels = games$game_id))
  final <- games$home_goals + games$away_goals - (games$result_type == "SO")

  expect_equal(as.vector(per_game), final)
  expect_true(all(goals$counts_as_goal))
  expect_false(any(goals$shootout))
})

test_that("liiga_goals can keep shootout and non-goal events", {
  local_mocked_bindings(liiga_get = mock_liiga)

  all_events <- liiga_goals(2025, include_shootout = TRUE, include_no_goal = TRUE)

  expect_true(any(all_events$shootout))
  expect_true(any(!all_events$counts_as_goal))
  expect_true(all(is.na(all_events$period_seconds[all_events$shootout])))
  expect_equal(nrow(liiga_goals(2025, game_id = 3)), sum(all_events$game_id == 3 & all_events$counts_as_goal & !all_events$shootout))
})

test_that("liiga_game returns rosters, goals and penalties", {
  local_mocked_bindings(liiga_get = mock_liiga)

  game <- liiga_game(400, 2025)

  expect_named(game, c("info", "rosters", "goals", "penalties", "referees"))
  expect_equal(nrow(game$info), 1)
  counted <- game$goals[game$goals$counts_as_goal & !game$goals$shootout, ]
  expect_equal(nrow(counted), game$info$home_goals + game$info$away_goals)
  expect_true("VT0" %in% game$goals$goal_types)
  expect_true(all(game$rosters$dressed))
  expect_setequal(unique(game$rosters$home_away), c("home", "away"))
  expect_true("G" %in% game$rosters$position)
  expect_false(anyNA(game$rosters$position))
  # bench penalty: no player, but someone serves it
  bench <- is.na(game$penalties$player_id)
  expect_equal(sum(bench), 1)
  expect_false(anyNA(game$penalties$player_name[!bench]))
  expect_false(anyNA(game$penalties$served_by_name))
  expect_gt(nrow(game$referees), 0)
})

test_that("liiga_shots normalises direction and names shooters", {
  local_mocked_bindings(liiga_get = mock_liiga)

  shots <- liiga_shots(400, 2025)

  expect_gt(nrow(shots), 0)
  expect_setequal(unique(shots$team), c("Kärpät", "KooKoo"))
  expect_false(anyNA(shots$shot_result))
  expect_false(anyNA(shots$shooter_name))
  expect_true(all(shots$shooter_id[shots$team == "Kärpät"] %in%
    liiga_game_rosters(400, 2025)$player_id))

  # both teams shoot towards positive x after flipping
  mean_x <- tapply(shots$x[shots$shot_result != "blocked"], shots$team[shots$shot_result != "blocked"], mean)
  expect_true(all(mean_x > 10))

  goals <- shots[shots$is_goal, ]
  expect_equal(nrow(goals), 3)
  expect_true(all(goals$shot_distance < 20))
})

test_that("liiga_boxscore sums periods into one row per player", {
  local_mocked_bindings(liiga_get = mock_liiga)

  box <- liiga_boxscore(400, 2025)

  expect_named(box, c("teams", "skaters", "goalies"))
  expect_equal(nrow(box$teams), 2)
  expect_equal(sum(box$teams$goals), 3)

  # team shot attempts match the shot map, goalie shots on goal match saves + goals
  shots <- liiga_shots(400, 2025)
  expect_equal(
    box$teams$shot_attempts[order(box$teams$team)],
    as.vector(table(shots$team)[sort(box$teams$team)])
  )
  expect_equal(sum(box$goalies$shots_on_goal), sum(shots$on_target))
  expect_false(any(duplicated(box$skaters$player_id)))
  expect_false(anyNA(box$skaters$player_name))
  expect_true(all(box$goalies$position == "G"))
  expect_equal(sum(box$skaters$goals, na.rm = TRUE), 3)
  expect_true(all(box$goalies$saves <= box$goalies$shots_on_goal))
})

test_that("liiga_goalie_stats aggregates box scores", {
  local_mocked_bindings(liiga_get = mock_liiga)

  goalies <- liiga_goalie_stats(2025, game_id = c(400, 400))

  expect_true(all(goalies$games_played == 2))
  expect_equal(sum(goalies$goals_against), 6)
  expect_true(all(goalies$save_pct > 0 & goalies$save_pct <= 1))
})

test_that("liiga_standings, skater stats, player and teams parse", {
  local_mocked_bindings(liiga_get = mock_liiga)

  standings <- liiga_standings(2025)
  expect_equal(standings$ranking, seq_len(nrow(standings)))
  expect_type(standings$power_play_percentage, "double")
  expect_error(liiga_standings(2025, "nope"), "Unknown table")

  skaters <- liiga_skater_stats(2025)
  expect_equal(nrow(skaters), 15)
  expect_false(is.unsorted(rev(skaters$points)))
  expect_false(anyNA(skaters$position))

  player <- liiga_player(29969180)
  expect_equal(nrow(player$bio), 1)
  expect_equal(player$bio$nationality, "FIN")
  expect_true(is.na(player$bio$height))
  expect_gt(nrow(player$seasons), 0)

  teams <- liiga_teams()
  expect_equal(nrow(teams), 3)
  expect_false(anyNA(teams$team_id))
})
