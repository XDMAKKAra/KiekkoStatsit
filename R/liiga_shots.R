#' Load Liiga shot data
#'
#' @param game_id One or more Liiga game ids
#' @param season End year of the season the games belong to
#'
#' @description Get every shot attempt of a game (goals, saves, misses and
#' blocked shots) with shooter, strength, players on ice, Liiga's own expected
#' goals value and rink coordinates.
#'
#' `x_raw` and `y_raw` are the coordinates exactly as Liiga sends them
#' (roughly 0-1000 by 0-508). `x` and `y` are converted to metres from center
#' ice and flipped so the shooting team always attacks towards positive `x`.
#' The conversion assumes a 60 x 30 m rink, so treat `x`, `y`,
#' `shot_distance` and `shot_angle` as approximate.
#'
#' Older games may come back empty: Liiga does not have shot locations for
#' every game.
#'
#' @return A tibble with one row per shot attempt
#' @export
#'
#' @examples
#' \dontrun{
#' shots <- liiga_shots(400, 2025)
#' dplyr::count(shots, team, shot_result)
#' }
liiga_shots <- function(game_id, season){
  dplyr::bind_rows(lapply(game_id, function(id) liiga_shots_single(id, season)))
}

liiga_shots_single <- function(game_id, season){

  shots <- liiga_get(glue::glue("shotmap/{season}/{game_id}"))

  if(length(shots) == 0){
    message(glue::glue("No shot data for game {game_id} ({season})"))
    return(dplyr::tibble())
  }

  game <- liiga_get(glue::glue("games/{season}/{game_id}"))

  parse_liiga_shots(shots, game)
}

parse_liiga_shots <- function(shots, game){

  rosters <- parse_liiga_rosters(game)
  g <- game$game

  teams <- c(g$homeTeam$teamName, g$awayTeam$teamName)
  names(teams) <- c(liiga_team_id(g$homeTeam$teamId), liiga_team_id(g$awayTeam$teamId))

  strength <- c(EvenStrengthShot = "EV", PowerplayShot = "PP", ShorthandedShot = "SH")
  result <- c(GOAL = "goal", GOALIE_BLOCKED = "saved", MISSED = "missed", PLAYER_BLOCKED = "blocked")

  df <- dplyr::bind_rows(lapply(shots, function(s){
    dplyr::tibble(
      game_id = g$id,
      season = nz(g$season),
      period = nz(s$period),
      game_seconds = nz(s$gameTime),
      period_seconds = nz(s$gameTime) - (nz(s$period) - 1) * 1200,
      team_id = nz(s$shootingTeamId),
      shooter_id = nz(s$shooterId),
      blocker_id = nz(s$blockerId),
      event_type = nz(s$eventType, NA_character_),
      shot_result = unname(result[nz(s$eventType, NA_character_)]),
      strength = unname(strength[nz(s$type, NA_character_)]),
      skaters_for = nz(s$ownTeamPlayersOnIce),
      skaters_against = nz(s$otherTeamPlayersOnIce),
      xg = nz(s$xg),
      attacking_right = identical(s$shootingTeamId, s$leftTeam),
      x_raw = nz(s$shotX),
      y_raw = nz(s$shotY)
    )
  }))

  df$team <- unname(teams[as.character(df$team_id)])

  player_names <- stats::setNames(rosters$player_name, rosters$player_id)
  df$shooter_name <- unname(player_names[as.character(df$shooter_id)])
  df$blocker_name <- unname(player_names[as.character(df$blocker_id)])

  df$is_goal <- df$shot_result %in% "goal"
  df$on_target <- df$shot_result %in% c("goal", "saved")
  df$fenwick <- !df$shot_result %in% "blocked"

  # the team listed as leftTeam shoots towards the right end (higher x_raw)
  x_dir <- ifelse(df$attacking_right, df$x_raw, 1000 - df$x_raw)
  y_dir <- ifelse(df$attacking_right, df$y_raw, 508 - df$y_raw)

  df$x <- round((x_dir - 500) * 60 / 1000, 2)
  df$y <- round((y_dir - 254) * 30 / 508, 2)

  # goal line is 4 m from the end boards
  df$shot_distance <- round(sqrt((26 - df$x)^2 + df$y^2), 2)
  df$shot_angle <- round(atan2(abs(df$y), 26 - df$x) * 180 / pi, 1)

  dplyr::relocate(df, "team", .before = "team_id") %>%
    dplyr::relocate("shooter_name", .after = "shooter_id") %>%
    dplyr::relocate("blocker_name", .after = "blocker_id") %>%
    dplyr::select(-"attacking_right")
}
