#' Load Liiga standings
#'
#' @param season End year of the season, e.g. `2026` for 2025-26
#' @param table Which table to return: `"season"` (regular season, default),
#' `"playoffs"`, `"playout"`, `"qualifications"` or `"valmistavat_ottelut"`
#' (preseason)
#'
#' @description Get the league table with points, goal difference, special
#' teams and penalty totals.
#'
#' @return A tibble with one row per team
#' @export
#'
#' @examples
#' \dontrun{
#' liiga_standings(2025)
#' }
liiga_standings <- function(season = liiga_current_season(), table = "season"){

  x <- liiga_get("standings/", list(season = season))

  if(!table %in% names(x)){
    stop(
      glue::glue("Unknown table '{table}'. Available: {paste(names(x), collapse = ', ')}"),
      call. = FALSE
    )
  }

  df <- records_to_tibble(x[[table]])

  if(nrow(df) == 0) return(df)

  df$season <- season

  df %>%
    dplyr::select(-dplyr::any_of(c("internal_id", "team_logos_dark_bg_original", "team_logos_light_bg_original"))) %>%
    dplyr::rename(dplyr::any_of(c(logo = "team_logos_light_bg", logo_dark = "team_logos_dark_bg"))) %>%
    dplyr::mutate(team_id = liiga_team_id(.data$team_id)) %>%
    dplyr::relocate("season", "ranking", "team_name", "team_id") %>%
    dplyr::relocate(dplyr::any_of(c("logo", "logo_dark")), .after = dplyr::last_col())
}

#' Load Liiga skater stats
#'
#' @inheritParams liiga_games
#'
#' @description Get season totals for every skater: goals, assists, plus-minus,
#' shots, expected goals, time on ice and faceoffs. Goalies are not included,
#' see [liiga_goalie_stats()].
#'
#' @return A tibble with one row per skater
#' @export
#'
#' @examples
#' \dontrun{
#' liiga_skater_stats(2025) %>%
#'   dplyr::arrange(dplyr::desc(points))
#' }
liiga_skater_stats <- function(season = liiga_current_season(), tournament = "runkosarja"){

  tournament <- liiga_tournament(tournament)

  x <- liiga_get(
    glue::glue("players/stats/summed/{season}/{season}/{tournament}/true"),
    list(dataType = "basicStats")
  )

  df <- records_to_tibble(x)

  if(nrow(df) == 0) return(df)

  df$season <- season
  df$player_name <- paste(df$first_name, df$last_name)
  df$position <- liiga_position(df$role)

  df %>%
    dplyr::select(-dplyr::any_of(c("picture_url", "previous_teams_for_tournament", "current", "last_season"))) %>%
    dplyr::relocate("season", "player_id", "player_name", "team_name", "position") %>%
    dplyr::arrange(dplyr::desc(.data$points), dplyr::desc(.data$goals))
}

#' Load a Liiga player's profile
#'
#' @param player_id Liiga player id, e.g. from [liiga_skater_stats()]
#'
#' @description Get a player's bio and season-by-season Liiga history for the
#' regular season, playoffs and other tournaments. Goalie rows include saves,
#' goals against and save percentage.
#'
#' @return A list of tibbles: `bio` (one row) and `seasons`
#' @export
#'
#' @examples
#' \dontrun{
#' liiga_player(23017821)$seasons
#' }
liiga_player <- function(player_id){

  x <- liiga_get(glue::glue("players/info/{player_id}"))

  bio <- dplyr::tibble(
    player_id = player_id,
    player_name = player_name(x$firstName, x$lastName),
    first_name = nz(x$firstName, NA_character_),
    last_name = nz(x$lastName, NA_character_),
    birth_date = as.Date(nz(x$dateOfBirth, NA_character_)),
    birth_place = nz(x$birthLocality$name, NA_character_),
    birth_country = nz(x$birthLocality$country$code, NA_character_),
    nationality = nz(x$nationality$code, NA_character_),
    handedness = nz(x$handedness, NA_character_),
    # 0 means unknown
    height = if(identical(x$height, 0L)) NA_integer_ else nz(x$height),
    weight = if(identical(x$weight, 0L)) NA_integer_ else nz(x$weight),
    suspended = isTRUE(x$isSuspended)
  )

  # tournaments can disagree on column types, so combine as text first
  seasons <- dplyr::bind_rows(lapply(names(x$historical), function(tournament){
    df <- records_to_tibble(x$historical[[tournament]])
    if(nrow(df) == 0) return(NULL)
    df$tournament <- NULL
    df[] <- lapply(df, as.character)
    dplyr::mutate(df, player_id = as.character(player_id), tournament = tournament, .before = 1)
  }))

  if(nrow(seasons) > 0){
    seasons[] <- lapply(seasons, utils::type.convert, as.is = TRUE)
    seasons <- dplyr::arrange(seasons, .data$tournament, .data$season)
  }

  list(bio = bio, seasons = seasons)
}

#' Load Liiga teams
#'
#' @description Get basic info for every Liiga team: id, names, home arena
#' capacity, website and logo.
#'
#' @return A tibble with one row per team
#' @export
#'
#' @examples
#' \dontrun{
#' liiga_teams()
#' }
liiga_teams <- function(){

  x <- liiga_get("teams/info")

  dplyr::bind_rows(lapply(names(x$teams), function(key){
    t <- x$teams[[key]]
    dplyr::tibble(
      team_id = as.numeric(nz(t$id)),
      team_key = key,
      team_name = nz(t$name, NA_character_),
      short_name = nz(t$short_name, NA_character_),
      slug = nz(t$slug, NA_character_),
      country = nz(t$country$code, NA_character_),
      venue_capacity = nz(t$current_venue_capacity),
      website = nz(t$url, NA_character_),
      logo = nz(t$logo, NA_character_)
    )
  })) %>%
    dplyr::arrange(.data$team_name)
}
