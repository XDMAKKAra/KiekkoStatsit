#' Load Liiga box scores
#'
#' @param game_id One or more Liiga game ids
#' @param season End year of the season the games belong to
#'
#' @description Get full-game player and team stats, summed over all periods.
#' Besides the usual counting stats this includes Liiga's tracking data where
#' it exists: time on ice, faceoffs by zone, corsi, zone starts, passes, skating
#' distance, top speed, hardest shot and expected goals.
#'
#' @return A list of tibbles: `teams`, `skaters` and `goalies`, one row per
#' team or player per game
#' @export
#'
#' @examples
#' \dontrun{
#' box <- liiga_boxscore(400, 2025)
#' box$skaters
#' }
liiga_boxscore <- function(game_id, season){

  games <- lapply(game_id, function(id){
    stats <- liiga_get(glue::glue("games/stats/{season}/{id}"))
    game <- liiga_get(glue::glue("games/{season}/{id}"))
    parse_liiga_boxscore(stats, game)
  })

  list(
    teams = dplyr::bind_rows(lapply(games, `[[`, "teams")),
    skaters = dplyr::bind_rows(lapply(games, `[[`, "skaters")),
    goalies = dplyr::bind_rows(lapply(games, `[[`, "goalies"))
  )
}

parse_liiga_boxscore <- function(stats, game){

  g <- game$game
  rosters <- parse_liiga_rosters(game)

  sides <- list(home = stats$homeTeam, away = stats$awayTeam)
  team_names <- list(home = g$homeTeam$teamName, away = g$awayTeam$teamName)

  # one row per player per period
  period_rows <- function(field){
    dplyr::bind_rows(lapply(names(sides), function(side){
      dplyr::bind_rows(lapply(sides[[side]], function(p){
        records <- lapply(p[[field]], function(x){
          period <- x$period
          x$period <- NULL
          c(x, period[setdiff(names(period), "period")])
        })
        df <- records_to_tibble(records)
        if(nrow(df) > 0) df$home_away <- side
        df
      }))
    }))
  }

  sum_players <- function(df){
    if(nrow(df) == 0) return(df)
    df %>%
      dplyr::group_by(.data$home_away, .data$player_id, .data$jersey_id) %>%
      dplyr::summarise(
        dplyr::across(dplyr::where(is.numeric), sum_na),
        dplyr::across(dplyr::where(is.logical), any),
        .groups = "drop"
      )
  }

  skaters <- sum_players(period_rows("periodPlayerStats"))
  goalies <- sum_players(period_rows("goaliePeriodStats"))

  # top speed and hardest shot are per game, not per period
  extras <- records_to_tibble(c(stats$homeTeamGamePlayerStats, stats$awayTeamGamePlayerStats))

  if(nrow(skaters) > 0){
    if(nrow(extras) > 0) skaters <- dplyr::left_join(skaters, extras, by = "player_id")
    skaters$faceoff_pct <- round(100 * skaters$faceoffs_won / skaters$faceoffs_total, 1)
    skaters$faceoff_pct[is.nan(skaters$faceoff_pct)] <- NA
  }

  if(nrow(goalies) > 0){
    goalies$save_pct <- round(goalies$saves / goalies$shots_on_goal, 3)
    goalies$save_pct[is.nan(goalies$save_pct)] <- NA
    goalies <- dplyr::select(goalies, -dplyr::any_of(c(
      "faceoffs_total", "faceoffs_won", "faceoffs_center_total", "faceoffs_center_won",
      "faceoffs_defence_total", "faceoffs_defence_won", "faceoffs_offence_total",
      "faceoffs_offence_won", "saves_percentage"
    )))
  }

  add_names <- function(df){
    if(nrow(df) == 0) return(df)
    df$game_id <- g$id
    df$season <- nz(g$season)
    df$team <- unlist(team_names[df$home_away])
    df$player_name <- rosters$player_name[match(df$player_id, rosters$player_id)]
    df$position <- rosters$position[match(df$player_id, rosters$player_id)]
    dplyr::rename(df, jersey = "jersey_id", time_on_ice = "timeofice", penalty_minutes = "penaltyminutes") %>%
      dplyr::relocate("game_id", "season", "team", "home_away", "player_id", "player_name", "position", "jersey")
  }

  teams <- dplyr::bind_rows(lapply(names(sides), function(side){
    records <- lapply(sides[[side]], function(p){
      p[c("periodPlayerStats", "goaliePeriodStats")] <- NULL
      # sent as a list of power play labels, e.g. ["B", "D"]
      p$powerPlayInstances <- length(p$powerPlayInstances)
      p$shortHandedInstances <- length(p$shortHandedInstances)
      p
    })
    df <- records_to_tibble(records)
    if(nrow(df) == 0) return(df)
    df %>%
      dplyr::select(-dplyr::any_of("period")) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), sum_na)) %>%
      # Liiga's team "shots" counts every attempt, blocked and missed included
      dplyr::rename(dplyr::any_of(c(shot_attempts = "shots"))) %>%
      dplyr::mutate(
        game_id = g$id,
        season = nz(g$season),
        team = team_names[[side]],
        home_away = side,
        .before = 1
      )
  }))

  list(teams = teams, skaters = add_names(skaters), goalies = add_names(goalies))
}

#' Liiga goalie season stats
#'
#' @inheritParams liiga_games
#' @param game_id Optional game ids. By default every finished game of the
#' season is used.
#'
#' @description Liiga's API has no goalie stat table, so this builds one from
#' the box score of every finished game. That is two requests per game, so a
#' full regular season takes a few minutes. Includes goals saved above
#' expected (`gsax`) based on Liiga's own expected goals.
#'
#' @return A tibble with one row per goalie and team
#' @export
#'
#' @examples
#' \dontrun{
#' liiga_goalie_stats(2025)
#' }
liiga_goalie_stats <- function(season = liiga_current_season(), tournament = "runkosarja",
                               game_id = NULL){

  if(is.null(game_id)){
    games <- liiga_games(season, tournament)
    if(nrow(games) == 0) return(dplyr::tibble())
    game_id <- games$game_id[games$ended %in% TRUE]
  }

  goalies <- dplyr::bind_rows(lapply(seq_along(game_id), function(i){
    if(i %% 25 == 0) message(glue::glue("Box score {i} of {length(game_id)}"))
    Sys.sleep(0.1)
    tryCatch(
      liiga_boxscore(game_id[i], season)$goalies,
      error = function(e){
        message(glue::glue("Skipping game {game_id[i]}: {conditionMessage(e)}"))
        NULL
      }
    )
  }))

  if(nrow(goalies) == 0) return(dplyr::tibble())

  goalies %>%
    dplyr::filter(.data$time_on_ice > 0) %>%
    dplyr::group_by(.data$player_id, .data$player_name, .data$team) %>%
    dplyr::summarise(
      games_played = dplyr::n(),
      time_on_ice = sum(.data$time_on_ice),
      shots_against = sum_na(.data$shots_on_goal),
      saves = sum_na(.data$saves),
      goals_against = sum_na(.data$goals_allowed),
      xg_against = sum_na(.data$expected_goals_against),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      save_pct = round(.data$saves / .data$shots_against, 3),
      gaa = round(.data$goals_against * 3600 / .data$time_on_ice, 2),
      gsax = round(.data$xg_against - .data$goals_against, 2)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$time_on_ice))
}
