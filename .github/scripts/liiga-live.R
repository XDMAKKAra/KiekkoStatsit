# Live check against liiga.fi. Fails if the API has changed shape.
library(KiekkoStatsit)

check <- function(label, ok){
  cat(sprintf("%-55s %s\n", label, if(isTRUE(ok)) "ok" else "FAILED"))
  if(!isTRUE(ok)) failed <<- c(failed, label)
}
failed <- character()

games <- liiga_games(2025)
check("liiga_games(2025) has a full regular season", nrow(games) >= 400)

goals <- liiga_goals(2025)
played <- games[games$ended, ]
per_game <- table(factor(goals$game_id, levels = played$game_id))
final <- played$home_goals + played$away_goals - (played$result_type == "SO")
check("liiga_goals(2025) adds up to every final score", all(as.vector(per_game) == final))

check("liiga_games(\"playoffs\") returns games", nrow(liiga_games(2025, "playoffs")) > 0)

game <- liiga_game(400, 2025)
check("liiga_game() returns dressed rosters", nrow(game$rosters) >= 36)

shots <- liiga_shots(400, 2025)
check("liiga_shots() returns located shots", nrow(shots) > 50 && !anyNA(shots$x))
check("liiga_shots() goals match the score", sum(shots$is_goal) == game$info$home_goals + game$info$away_goals)

box <- liiga_boxscore(400, 2025)
check("liiga_boxscore() shot attempts match shots",
      sum(box$teams$shot_attempts) == nrow(shots))

check("liiga_goalie_stats() works on a few games",
      nrow(liiga_goalie_stats(2025, game_id = played$game_id[1:5])) >= 2)

check("liiga_standings() has every team", nrow(liiga_standings(2025)) >= 15)
skaters <- liiga_skater_stats(2025)
check("liiga_skater_stats() returns skaters", nrow(skaters) > 300)
check("liiga_player() returns history", nrow(liiga_player(skaters$player_id[1])$seasons) > 0)
check("liiga_teams() returns teams", nrow(liiga_teams()) >= 15)

current <- liiga_games()
check(sprintf("current season (%s) has a schedule", liiga_current_season()), nrow(current) > 0)

if(length(failed) > 0) stop("Liiga live check failed: ", paste(failed, collapse = "; "))
