#' Simulation of a league with fixed lineups
#' @param lineups (N x 5)-dim. matrix of lineups where N=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param reps integer(1), number of replications of a league (if method="sample")
#' @param ncores integer(1), number of processor cores for parallelization
#' @param method character(1), either "expected" or "sample" (default)
#'   More details in game().
#' @param repsGame integer(1), number of repetitions of each game (if method="sample").
#'   Default is 1 (i.e. no repetitions)
#' @return list of class "simResult" with:
#'   * results: data.table with the results of every game
#'   * summary: data.table with points won by the teams
#'   * lineups: the lineups input parameter matrix
#' @export
#'
#' @examples
#' lineups <- cbind(t = c(4,0,0,4), a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5))
#' rownames(lineups) <- c("Nob", "FdS", "USV", "Marco")
#'
#' set.seed(2020)
#' (res <- simLeague(lineups, reps = 100))
#' str(res)
#'
#' # For less variability in the results, the whole league can be played more often
#' # (increase reps) or each game can be replicated repsGame-times which results in average
#' # game results with an average score and average points.
#' (res <- simLeague(lineups, reps = 10, method = "sample", repsGame = 20))
#' (res <- simLeague(lineups, reps = 100, method = "sample", repsGame = 20))
#' # Or, method="expected" can be used:
#' (res <- simLeague(lineups, method = "expected"))
#' res$results        # score not necessarily integer-valued for method="expected".
#' str(res)           # only 1 leagueID, i.e. one run. No sampling of results.
#'
simLeague <- function(lineups, reps = 1000, ncores = 8, method = "sample", repsGame = 1) {
  # unique team names required:
  if(length(unique(rownames(lineups))) != nrow(lineups))
    stop("Team names not unique.")
  checkmate::assertCharacter(method, pattern = "(expected)|(sample)")

  if(method == "sample") {
    cl <- makeCluster(rep("localhost", ncores), type = "SOCK")
    clusterSetRNGStream(cl)
    clusterExport(cl, list("reps", "lineups", "league", "game", "method", "repsGame",
                           "data.table", ":=", "as.data.table"),
                  envir = environment())
    res <- parLapply(cl, 1:reps, function(x) league(lineups, method = method, repsGame = repsGame))
    stopCluster(cl)
    runs <- reps
  }
  if(method == "expected") {
    res <- list(league(lineups, method = "expected"))
    runs <- 1L
  }

  resPoints  <- lapply(res, "[[", "points")
  resResults <- lapply(res, "[[", "results")

  teams <- resPoints[[1]]$lineup
  resPoints <- t(do.call(rbind, lapply(resPoints, "[[", "points")))
  rownames(resPoints) <- teams
  points    <- rowMeans(resPoints)   # average points
  wonNumber <- table(factor(teams[apply(resPoints, 2, which.max)], levels = teams))[teams]
                                     # number     of leagues won
  wonProp   <- wonNumber / runs      # proportion of leagues won
  resPointsEval   <- cbind(pointsAvg = points, wonNumber = wonNumber, wonProp = wonProp)
  resPointsEval   <- as.data.table(cbind(team = rownames(resPointsEval), as.data.frame(resPointsEval)))

  resResults <- do.call(rbind, resResults)
  resResults[, leagueID := as.character(rep(1:runs, each = nrow(res[[1]]$results)))]

  retVal <- list(results = resResults, summary = resPointsEval, lineups = lineups)
  class(retVal) <- "simResult"
  return(retVal)
}


