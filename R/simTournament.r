#' Simulation of a tournament with fixed lineups
#' @param lineups (N x 5)-dim. matrix of lineups where N=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param reps integer(1), number of replications of single tournament
#' @param method character(1), either "expected" or "sample" (default)
#'   More details in game().
#' @param repsGame integer(1), number of repetitions of each game (if method="sample").
#'   Default is 1 (i.e. no repetitions). repsGame > 1 is NOT recommended for tournaments.
#' @return list of class "simResult" with:
#'   * results: data.table with the results of every game
#'   * summary: data.table with points won by the teams
#'   * lineups: the lineups input parameter matrix
#'   * resultsTournament: list of created tournament objects
#' @export
#'
#' @examples
#' # define some lineups:
#' lineups <- cbind(t = c(4,0,0,4), a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5))
#' rownames(lineups) <- c("Nob", "FdS", "USV", "Marco")
#' lineups
#'
#' set.seed(2020)
#' (res <- simTournament(lineups, reps = 100))
#' res$results                # 300 = reps * (3 games per tournament with 4 teams) games alltogether
#' res$resultsTournament[[1]] # the 1st of reps tournaments
#' names(res)
#' str(res[1:3])
#' str(head(res$resultsTournament, 2))
#'
#' (res <- simTournament(lineups, method = "expected"))  # no sampling of results
#'
simTournament <- function(lineups, reps = 100, method = "sample", repsGame = 1) {
  checkmate::assertCharacter(method, pattern = "(expected)|(sample)")

  if(method == "sample") {
    res <- lapply(1:reps, function(x) tournament(lineups, method = "sample", repsGame = repsGame))
    runs <- reps
  }
  if(method == "expected") {
    res <- list(tournament(lineups, method = "expected"))
    runs <- 1L
  }

  # list of tournament objects to 1 data.table:
  X <- lapply(res, as.data.table.tournament)
  for(i in seq_along(X))
    X[[i]][, tournamentID := i]
  X <- do.call(rbind, X)

  stages <- names(res[[1]])
  resPoints <- do.call(cbind, lapply(res, points))
  numStages <- max(resPoints)
  points    <- rowMeans(resPoints)                                  # average points
  wonNumber <- apply(resPoints, 1, function(x) sum(x == numStages)) # number     of tournaments won
  wonProp   <- wonNumber / runs                                     # proportion of tournaments won
  resPointsEval <- cbind(pointsAvg = points, wonNumber = wonNumber, wonProp = wonProp)
  resPointsEval <- as.data.table(cbind(team = rownames(resPointsEval), as.data.frame(resPointsEval)))

  retVal <- list(results = X, summary = resPointsEval, lineups = lineups, resultsTournament = res)
  class(retVal) <- "simResult"
  gc()
  return(retVal)
}
