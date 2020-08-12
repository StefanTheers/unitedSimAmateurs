#' Simulation of a tournament with a fixed \code{lineups} matrix
#'
#' If \code{method="sample"}, the better team does not necessarily win
#' every game of a stage of a tournament. That is why \code{simTournament}
#' calls \code{\link{tournament}} \code{reps}-times and returns
#' an easy-to-read summary.
#'
#' @param lineups [\code{matrix}]\cr
#'   (\code{N} x 5)-dim. matrix of lineups where \code{N}=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param reps [\code{integer(1)}]\cr
#'   number of replications of the single tournament
#' @param method [\code{character(1)}]\cr
#'   either \code{"expected"} or \code{"sample"} (default).
#'   More details in \code{\link{game}}.
#' @param repsGame [\code{integer(1)}]\cr
#'   number of repetitions of each game (if \code{method="sample"}).
#'   Default is 1 (i.e. no repetitions). \code{repsGame} > 1 is NOT recommended for tournaments.
#' @return list of S3 class \code{"simResult"} with:
#' \describe{
#'   \item{\code{results}}{  [\code{data.table}]\cr results of every game }
#'   \item{\code{summary}}{  [\code{data.table}]\cr points won by the teams }
#'   \item{\code{lineups}}{  [\code{matrix}]\cr the \code{lineups} input parameter matrix }
#'   \item{\code{resultsTournament}}{  [\code{list}]\cr all created \code{\link{tournament}} objects }
#' }
#' @seealso \code{\link{tournament}}, \code{\link{simTournamentPerm}}, \code{\link{simAmateurs}}
#' @export
#'
#' @examples
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
