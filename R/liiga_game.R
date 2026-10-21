#' Load a single Liiga game
#'
#' @param game_id Liiga game id, from [liiga_games()]
#' @param season End year of the season the game belongs to
#'
#' @description Get the details of one game: game info, both lineups, goals
#' (including on-ice jersey numbers), penalties and referees.
#'
#' @return A list of tibbles: `info`, `rosters`, `goals`, `penalties`, `referees`
#' @export
#'
#' @examples
#' \dontrun{
#' game <- liiga_game(400, 2025)
#' game$penalties
#' }
liiga_game <- function(game_id, season){

  x <- liiga_get(glue::glue("games/{season}/{game_id}"))

  rosters <- parse_liiga_rosters(x)

  goals <- filter_liiga_goals(
    parse_liiga_goals(x$game),
    include_shootout = TRUE,
    include_no_goal = TRUE
  )

  list(
    info = parse_liiga_game_row(x$game),
    rosters = rosters[rosters$dressed, ],
    goals = goals,
    penalties = parse_liiga_penalties(x$game, rosters),
    referees = parse_liiga_referees(x$game)
  )
}

#' Load Liiga game lineups
#'
#' @inheritParams liiga_game
#' @param game_id One or more Liiga game ids
#'
#' @param dressed_only Keep only players dressed for the game? Liiga also
#' lists the rest of the squad, with no line. Defaults to `TRUE`.
#'
#' @description Get the players of both teams, with line, position, jersey
#' number and bio details.
#'
#' @return A tibble with one row per player per game
#' @export
#'
#' @examples
#' \dontrun{
#' liiga_game_rosters(400, 2025)
#' }
liiga_game_rosters <- function(game_id, season, dressed_only = TRUE){
  rosters <- dplyr::bind_rows(lapply(game_id, function(id){
    parse_liiga_rosters(liiga_get(glue::glue("games/{season}/{id}")))
  }))
  if(dressed_only && nrow(rosters) > 0) rosters <- rosters[rosters$dressed, ]
  rosters
}

parse_liiga_rosters <- function(x){

  sides <- list(home = x$homeTeamPlayers, away = x$awayTeamPlayers)

  dplyr::bind_rows(lapply(names(sides), function(side){
    dplyr::bind_rows(lapply(sides[[side]], function(p){
      dplyr::tibble(
        game_id = nz(x$game$id),
        season = nz(x$game$season),
        team = nz(p$teamName, NA_character_),
        team_id = liiga_team_id(p$teamId),
        home_away = side,
        player_id = nz(p$id),
        player_name = player_name(p$firstName, p$lastName),
        first_name = nz(p$firstName, NA_character_),
        last_name = nz(p$lastName, NA_character_),
        jersey = nz(p$jersey),
        line = nz(p$line),
        dressed = !is.null(p$line),
        position = liiga_position(nz(p$roleCode, NA_character_)),
        role = nz(p$role, NA_character_),
        role_code = nz(p$roleCode, NA_character_),
        captain = isTRUE(p$captain),
        alternate_captain = isTRUE(p$alternateCaptain),
        rookie = isTRUE(p$rookie),
        handedness = nz(p$handedness, NA_character_),
        height = nz(p$height),
        weight = nz(p$weight),
        birth_date = as.Date(nz(p$dateOfBirth, NA_character_)),
        nationality = nz(p$nationality, NA_character_)
      )
    }))
  }))
}

parse_liiga_penalties <- function(g, rosters){

  sides <- list(home = g$homeTeam, away = g$awayTeam)

  penalties <- dplyr::bind_rows(lapply(names(sides), function(side){
    team_info <- sides[[side]]
    dplyr::bind_rows(lapply(team_info$penaltyEvents, function(e){
      dplyr::tibble(
        game_id = g$id,
        season = nz(g$season),
        event_id = nz(e$eventId),
        period = nz(e$period),
        game_seconds = nz(e$gameTime),
        period_seconds = nz(e$gameTime) - (nz(e$period) - 1) * 1200,
        team = nz(team_info$teamName, NA_character_),
        home_away = side,
        # player id 0 is a bench penalty, e.g. too many men
        player_id = if(identical(e$playerId, 0L)) NA_integer_ else nz(e$playerId),
        served_by_id = nz(e$suffererPlayerId),
        penalty_minutes = nz(e$penaltyMinutes),
        penalty_code = nz(e$penaltyFaultType, NA_character_),
        penalty_name = nz(e$penaltyFaultName, NA_character_),
        penalty_start = nz(e$penaltyBegintime),
        penalty_end = nz(e$penaltyEndtime)
      )
    }))
  }))

  if(nrow(penalties) == 0 || nrow(rosters) == 0) return(penalties)

  names <- rosters[, c("player_id", "player_name")]
  penalties$player_name <- names$player_name[match(penalties$player_id, names$player_id)]
  penalties$served_by_name <- names$player_name[match(penalties$served_by_id, names$player_id)]

  penalties %>%
    dplyr::relocate("player_name", .after = "player_id") %>%
    dplyr::relocate("served_by_name", .after = "served_by_id")
}

parse_liiga_referees <- function(g){
  dplyr::bind_rows(lapply(g$referees, function(r){
    dplyr::tibble(
      game_id = g$id,
      official_id = nz(r$officialID),
      official_name = player_name(r$firstName, r$lastName),
      role = nz(r$roleName, NA_character_),
      role_code = nz(r$roleAbbrv, NA_character_)
    )
  }))
}

# Liiga role codes: KH center, VL/OL left/right wing, H forward,
# VP/OP left/right defense, P and "7. P" defense, MV goalie
liiga_position <- function(code){
  lookup <- c(
    KH = "C", VL = "LW", OL = "RW", H = "F",
    VP = "D", OP = "D", P = "D", "7. P" = "D", MV = "G"
  )
  unname(lookup[code])
}
