#' Play league.
#' @param lineups (N x 5)-dim. matrix of lineups where N=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param method character(1), either "expected" or "sample" (default).
#'   More details in game().
#' @param repsGame integer(1), number of repetitions of each game (if method="sample").
#'   Default is 1 (i.e. no repetitions).
#' @return list of class "league" that contains:
#'   * points, a data.table with the points reached by each team
#'   * results, a data.table with the full-time results
#'   * resultsFull, a data.table with all full-time results (i.e. repetitions of the very same game
#'     are included if repsGame > 1).
#' @note
#' * No home or away games are supported.
#' * If you use method="sample" and repsGame > 1, league$results will contain non-integer scores and points.
#' @export
#'
#' @examples
#' lineups <- cbind(t = c(4,0,0,4), a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5))
#' rownames(lineups) <- c("Nob", "FdS", "USV", "Marco")
#' lineups
#'
#' set.seed(2020)
#' (res <- league(lineups))
#' str(res)
#' res$results
#' league(lineups, method = "sample")                 # different from 1st run
#' league(lineups, method = "expected")               # always the same
#' league(lineups, method = "sample", repsGame = 20)  # 20x repetition of each game => more exact results
#' league(lineups, method = "sample", repsGame = 20)  # close to the previous league simulation
#'
league <- function(lineups, method = "sample", repsGame = 1) {
  # define league games:
  games <- t(combn(rownames(lineups), m = 2))
  if(method == "sample")
    games <- games[rep(1:nrow(games), each = repsGame), ]

  # play league:
  res <- t(apply(games, 1, function(x) {
    attr(game(x1 = lineups[x[1], ], x2 = lineups[x[2], ], etps = FALSE, method = method), "fullTimeScore")
  }))
  res <- data.table(games, res)
  colnames(res) <- c("team1", "team2", "score1", "score2")
  res[, points1 := 3 * (score1 > score2) + (score1 == score2)]
  res[, points2 := 3 * (score1 < score2) + (score1 == score2)]
  res2 <- res[, lapply(.SD, mean), by = team1:team2, .SDcols = score1:points2]

  # sum up points for each team:
  pointsTab <- data.table(lineup = rownames(lineups), points = 0)
  for(team in pointsTab[, lineup])
    pointsTab[lineup == team, points := sum(res2[team1 == team, points1]) +
                                        sum(res2[team2 == team, points2])]

  retVal <- list(points = as.data.table(pointsTab), results = res2[, team1:score2], resultsFull = res)
  class(retVal) <- "league"
  gc()
  return(retVal)
}

#' @export
print.league <- function(x) {
  print(x$points[order(points, decreasing = TRUE), ])
}

