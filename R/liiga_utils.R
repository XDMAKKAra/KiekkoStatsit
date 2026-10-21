#' Current Liiga season
#'
#' @param date A date. Defaults to today.
#'
#' @description Liiga seasons are named by the year they end in, so the
#' 2025-26 season is `2026`. From July onwards the next season is returned.
#'
#' @return An integer season, e.g. `2026`
#' @export
#'
#' @examples
#' liiga_current_season(as.Date("2025-10-01"))
liiga_current_season <- function(date = Sys.Date()){

  date <- as.Date(date)
  year <- as.integer(format(date, "%Y"))

  if(as.integer(format(date, "%m")) >= 7) year + 1L else year
}

# base url can be overridden with options(kiekkostatsit.liiga_url = ...)
liiga_base_url <- function(){
  getOption("kiekkostatsit.liiga_url", "https://liiga.fi/api/v2")
}

liiga_get <- function(path, query = NULL){

  url <- paste0(liiga_base_url(), "/", path)

  res <- httr::GET(
    url,
    query = query,
    httr::user_agent("KiekkoStatsit R package (https://github.com/XDMAKKAra/KiekkoStatsit)")
  )

  if(httr::http_error(res)){
    stop(
      glue::glue("Liiga API request failed with status {httr::status_code(res)}: {res$url}"),
      call. = FALSE
    )
  }

  jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

# accepted tournament names, with English aliases
liiga_tournament <- function(tournament){

  aliases <- c(
    runkosarja = "runkosarja", regular = "runkosarja",
    playoffs = "playoffs", pudotuspelit = "playoffs",
    valmistavat_ottelut = "valmistavat_ottelut", preseason = "valmistavat_ottelut"
  )

  tournament <- tolower(tournament[1])

  if(!tournament %in% names(aliases)){
    stop(
      glue::glue(
        "Unknown tournament '{tournament}'. Use one of: ",
        "{paste(unique(names(aliases)), collapse = ', ')}"
      ),
      call. = FALSE
    )
  }

  unname(aliases[tournament])
}

# NULL-safe scalar: NULL or empty becomes NA
nz <- function(x, default = NA){
  if(is.null(x) || length(x) == 0) default else x
}

# "168761288:hifk" -> 168761288
liiga_team_id <- function(x){
  if(is.null(x)) return(NA_real_)
  as.numeric(sub(":.*$", "", x))
}

liiga_time <- function(x){
  if(is.null(x)) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}

liiga_result_type <- function(x){
  switch(
    nz(x, ""),
    ENDED_DURING_REGULAR_GAME_TIME = "REG",
    ENDED_DURING_EXTENDED_GAME_TIME = "OT",
    ENDED_DURING_WINNING_SHOT_COMPETITION = "SO",
    NA_character_
  )
}

# camelCase -> snake_case
to_snake <- function(x){
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  tolower(gsub("^_|_$", "", x))
}

# keep the scalar fields of a record, flattening one level of nesting
flatten_record <- function(x, prefix = NULL){

  out <- list()

  for(name in names(x)){
    value <- x[[name]]
    key <- if(is.null(prefix)) name else paste0(prefix, "_", name)

    if(is.null(value)){
      out[[key]] <- NA
    } else if(is.list(value)){
      if(is.null(prefix) && !is.null(names(value))){
        out <- c(out, flatten_record(value, key))
      }
    } else if(length(value) == 1){
      out[[key]] <- value
    }
  }

  out
}

records_to_tibble <- function(records){

  if(length(records) == 0) return(dplyr::tibble())

  df <- dplyr::bind_rows(lapply(records, function(r) dplyr::as_tibble(flatten_record(r))))
  names(df) <- to_snake(names(df))

  # numbers sent as strings, e.g. "0.17821783"
  df[] <- lapply(df, function(col){
    if(is.character(col)){
      num <- suppressWarnings(as.numeric(col))
      if(all(is.na(num) == is.na(col))) return(num)
    }
    col
  })

  df
}

sum_na <- function(x){
  if(all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

player_name <- function(first, last){
  name <- trimws(paste(nz(first, ""), nz(last, "")))
  if(name == "") NA_character_ else name
}
