# Fixtures are trimmed real responses from https://liiga.fi/api/v2
read_fixture <- function(name){
  jsonlite::read_json(test_path("fixtures", paste0(name, ".json")))
}

# replaces liiga_get() so tests never touch the network
mock_liiga <- function(path, query = NULL){
  if(path == "games") return(read_fixture("games"))
  if(grepl("^games/stats/", path)) return(read_fixture("gamestats"))
  if(grepl("^games/", path)) return(read_fixture("game"))
  if(grepl("^shotmap/", path)) return(read_fixture("shotmap"))
  if(grepl("^standings", path)) return(read_fixture("standings"))
  if(grepl("^players/stats/", path)) return(read_fixture("skaters"))
  if(grepl("^players/info/", path)) return(read_fixture("player"))
  if(path == "teams/info") return(read_fixture("teams"))
  stop("no fixture for ", path)
}
