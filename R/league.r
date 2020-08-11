#' Play league
#'
#' Play a league with teams defined in the \code{lineups} matrix.
#'
#' @param lineups [\code{matrix}]\cr
#'   (\code{N} x 5)-dim. matrix of lineups where \code{N}=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param method [\code{character(1)}]\cr
#'   Either \code{"expected"} or \code{"sample"} (default).
#'   More details in \code{\link{game}}.
#' @param repsGame [\code{integer(1)}]\cr
#'   Number of repetitions of each game (if \code{method="sample"}).
#'   Default is 1 (i.e. no repetitions).
#' @return  object of S3 class \code{league}, a \code{list} with:
#'   \describe{
#'     \item{\code{points}}{  [\code{data.table}]\cr points reached by each team }
#'     \item{\code{results}}{ [\code{data.table}]\cr full-time results }
#'     \item{\code{resultsFull}}{  [\code{data.table}]\cr all full-time results (i.e. repetitions of the
#'                                  very same game are included if \code{repsGame} > 1) }
#'   }
#' @note
#' \itemize{
#'   \item No support for home or away games.
#'   \item If you use \code{method="sample"} and \code{repsGame} > 1, \code{league$results}
#'     will contain non-integer scorelines and points.
#'   \item The \code{\link{print.league}} method displays the results of the teams ordered by points.
#' }
#' @seealso \code{\link{print.league}}, \code{\link{game}}, \code{\link{simLeague}}, \code{\link{simAmateurs}}
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


#' \code{print} method for \code{league} objects
#' @param x [object of S3 class \code{"league"}]\cr
#'   the return value of \code{\link{league}}
#' @return \code{invisible(x$points)} ordered by \code{points}
#' @seealso \code{\link{league}}
#' @export
print.league <- function(x) {
  retVal <- x$points[order(points, decreasing = TRUE), ]
  print(retVal)
  return(invisible(retVal))
}

