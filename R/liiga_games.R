#' Load Liiga schedule and results
#'
#' @param season End year of the season, e.g. `2026` for 2025-26
#' @param tournament One of `"runkosarja"` (regular season, default),
#' `"playoffs"` or `"valmistavat_ottelut"` (preseason). English aliases
#' `"regular"` and `"preseason"` also work.
#'
#' @description Get every game of a Liiga season with the final score, expected
#' goals, special teams numbers, attendance and arena. Future games are included
#' with `ended = FALSE`.
#'
#' @return A tibble with one row per game
#' @export
#'
#' @examples
#' \dontrun{
#' liiga_games(2025)
#' liiga_games(2025, "playoffs")
#' }
liiga_games <- function(season = liiga_current_season(), tournament = "runkosarja"){

  tournament <- liiga_tournament(tournament)

  games <- liiga_get("games", list(tournament = tournament, season = season))

  if(length(games) == 0){
    message(glue::glue("No Liiga games found for {tournament} {season}"))
    return(dplyr::tibble())
  }

  dplyr::bind_rows(lapply(games, parse_liiga_game_row))
}

parse_liiga_game_row <- function(g){

  h <- g$homeTeam
  a <- g$awayTeam

  dplyr::tibble(
    game_id = g$id,
    season = nz(g$season),
    tournament = tolower(nz(g$serie)),
    game_week = nz(g$gameWeek),
    start_time = liiga_time(g$start),
    home_team = nz(h$teamName, NA_character_),
    away_team = nz(a$teamName, NA_character_),
    home_team_id = liiga_team_id(h$teamId),
    away_team_id = liiga_team_id(a$teamId),
    home_goals = nz(h$goals),
    away_goals = nz(a$goals),
    home_xg = nz(h$expectedGoals),
    away_xg = nz(a$expectedGoals),
    home_pp_goals = nz(h$powerplayGoals),
    home_pp_opportunities = nz(h$powerplayInstances),
    away_pp_goals = nz(a$powerplayGoals),
    away_pp_opportunities = nz(a$powerplayInstances),
    started = nz(g$started),
    ended = nz(g$ended),
    result_type = liiga_result_type(g$finishedType),
    game_seconds = nz(g$gameTime),
    spectators = nz(g$spectators),
    arena = nz(g$iceRink$name, NA_character_),
    arena_city = nz(g$iceRink$city, NA_character_)
  )
}

#' Load Liiga goals
#'
#' @inheritParams liiga_games
#' @param game_id Optional game ids to keep. By default every game is returned.
#' @param include_shootout Keep the shootout winner (period 5 of a regular
#' season game)? Defaults to `FALSE`.
#' @param include_no_goal Keep events that did not count: missed penalty
#' shots (`RL0`) and goals overturned on video review (`VT0`)? Defaults to
#' `FALSE`.
#'
#' @description Every goal of a season in one request, with scorer, assists,
#' score after the goal and goal type flags. Liiga goal type codes are kept in
#' `goal_types`: `YV` power play, `YV2` 5-on-3 power play, `AV` short-handed,
#' `TM` empty net, `IM` extra attacker, `SR` delayed penalty, `RL` penalty
#' shot, `VT` video review, `VL` shootout winner.
#'
#' @return A tibble with one row per goal
#' @export
#'
#' @examples
#' \dontrun{
#' goals <- liiga_goals(2025)
#' dplyr::count(goals, scorer_name, sort = TRUE)
#' }
liiga_goals <- function(season = liiga_current_season(), tournament = "runkosarja",
                        game_id = NULL, include_shootout = FALSE, include_no_goal = FALSE){

  tournament <- liiga_tournament(tournament)

  games <- liiga_get("games", list(tournament = tournament, season = season))

  if(!is.null(game_id)){
    games <- Filter(function(g) g$id %in% game_id, games)
  }

  goals <- dplyr::bind_rows(lapply(games, parse_liiga_goals))

  filter_liiga_goals(goals, include_shootout, include_no_goal)
}

filter_liiga_goals <- function(goals, include_shootout, include_no_goal){

  if(nrow(goals) == 0) return(goals)

  if(!include_shootout) goals <- goals[!goals$shootout, ]
  if(!include_no_goal) goals <- goals[goals$counts_as_goal, ]

  goals
}

parse_liiga_goals <- function(g){

  shootout_game <- identical(g$finishedType, "ENDED_DURING_WINNING_SHOT_COMPETITION")

  sides <- list(home = list(g$homeTeam, g$awayTeam), away = list(g$awayTeam, g$homeTeam))

  dplyr::bind_rows(lapply(names(sides), function(side){

    team_info <- sides[[side]][[1]]
    opponent_info <- sides[[side]][[2]]

    dplyr::bind_rows(lapply(team_info$goalEvents, function(e){

      types <- unlist(e$goalTypes)
      assists <- e$assistantPlayers
      shootout <- shootout_game && nz(e$period, 0) >= 5
      period_seconds <- if(shootout) NA_real_ else e$gameTime - (e$period - 1) * 1200

      dplyr::tibble(
        game_id = g$id,
        season = nz(g$season),
        event_id = nz(e$eventId),
        period = nz(e$period),
        game_seconds = nz(e$gameTime),
        period_seconds = period_seconds,
        team = nz(team_info$teamName, NA_character_),
        team_id = liiga_team_id(team_info$teamId),
        opponent = nz(opponent_info$teamName, NA_character_),
        home_away = side,
        scorer_id = nz(e$scorerPlayerId),
        scorer_name = player_name(e$scorerPlayer$firstName, e$scorerPlayer$lastName),
        assist_1_id = if(length(assists) >= 1) nz(assists[[1]]$playerId) else NA,
        assist_1_name = if(length(assists) >= 1) player_name(assists[[1]]$firstName, assists[[1]]$lastName) else NA_character_,
        assist_2_id = if(length(assists) >= 2) nz(assists[[2]]$playerId) else NA,
        assist_2_name = if(length(assists) >= 2) player_name(assists[[2]]$firstName, assists[[2]]$lastName) else NA_character_,
        home_score = nz(e$homeTeamScore),
        away_score = nz(e$awayTeamScore),
        goal_types = paste(types, collapse = ","),
        counts_as_goal = !any(types %in% c("RL0", "VT0")),
        power_play = any(types %in% c("YV", "YV2")),
        short_handed = "AV" %in% types,
        empty_net = "TM" %in% types,
        extra_attacker = "IM" %in% types,
        penalty_shot = any(types %in% c("RL", "RL0")),
        shootout = shootout,
        game_winning_goal = isTRUE(e$winningGoal),
        plus_jerseys = nz(e$plusPlayerIds, NA_character_),
        minus_jerseys = nz(e$minusPlayerIds, NA_character_)
      )
    }))
  }))
}
