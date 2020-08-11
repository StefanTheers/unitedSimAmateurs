#' Simulation of a league
#'
#' @param lineups [\code{matrix}]\cr
#'   (\code{N} x 5)-dim. matrix of lineups.
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param reps [\code{integer(1)}]\cr
#'   number of replications of a league (if \code{method="sample"})
#' @param ncores [\code{integer(1)}]\cr
#'   number of processor cores for parallelization
#' @param method [\code{character(1)}]\cr
#'   either \code{"expected"} or \code{"sample"} (default).
#'   More details in \code{\link{game}}.
#' @param repsGame [\code{integer(1)}]\cr
#'   number of repetitions of each game (if \code{method="sample"}).
#'   Default is 1 (i.e. no repetitions)
#' @return list of S3 class \code{"simResult"} with:
#' \describe{
#'   \item{\code{results}}{  [\code{data.table}]\cr results of every game }
#'   \item{\code{summary}}{  [\code{data.table}]\cr points won by the teams }
#'   \item{\code{lineups}}{  [\code{matrix}]\cr the \code{lineups} input parameter matrix }
#' }
#' @seealso \code{\link{league}}, \code{\link{simAmateurs}}
#' @export
#'
#' @examples
#' lineups <- cbind(t = c(4,0,0,4), a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5))
#' rownames(lineups) <- c("Nob", "FdS", "USV", "Marco")
#' lineups
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


