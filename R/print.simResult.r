#' \code{print} method for \code{simResult} objects
#'
#' @param x [object of S3 class \code{"simResult"}]\cr
#'   result of a league or tournament simulation
#' @param printNum [\code{logical(1)}]\cr
#'   Print (and calculate) the numbers in \code{num}?
#' @return \code{invisible(list)} with:
#'   \describe{
#'     \item{\code{summary}}{  [\code{data.table}]\cr results summary and lineups (if available) }
#'     \item{\code{nums}}{  [\code{integer(6)}]\cr with the number of teams, tournaments, leagues, games, games with extra-time and games with penalty shoot-outs }
#'   }
#'
#' @seealso \code{\link{simLeague}}, \code{\link{simTournament}}, \code{\link{simTournamentPerm}}, \code{\link{simAmateurs}}
#' @export
#'
print.simResult <- function(x, printNum = TRUE) {
  pointsAvgVar <- colnames(x$summary)[grep("^((pointsAvg)|(leaguePointsAvg))$", colnames(x$summary))]
  if(!any(names(x) == "lineups")) {
    # Note: Lineups may have to be kept secret. Thus, it should be allowed to delete them.
    X <- x$summary[order(X[, ..pointsAvgVar], decreasing = TRUE)]
    X$lineup <- NA
    print(X)
    return(invisible(X))
  }
  teamVec <- rownames(x$lineups)
  X <- as.data.table(x$lineups)
  X$team <- teamVec
  X <- merge(x$summary, X, by = "team")
  X <- X[order(X[, ..pointsAvgVar], decreasing = TRUE)]
  print(X)
  if(printNum) {
    cat("\nnumber of teams:\t\t\t",                      a1 <- length(unique(do.call(c, x$results[, team1:team2]))))
    if("tournamentID" %in% colnames(x$results)) {
      cat("\nnumber of tournaments:\t\t\t",              a2 <- length(unique(x$results[, tournamentID])))
      cat("\nnumber of games:\t\t\t",                    a3 <- nrow(x$results))
      cat("\nnumber of games with extra-time:\t",        a4 <- x$results[, sum(!is.na(scoreET1))])
      cat("\nnumber of games with penalty shoot-out:\t", a5 <- x$results[, sum(!is.na(scorePen1))], "\n\n")
      return(invisible(list(summary = X,
        nums = c(numTeams = a1, numTournaments = a2, numLeagues = 0, numGames = a3, numET = a4, numPen = a5))))
    }
    if("leagueID" %in% colnames(x$results)) {
      cat("\nnumber of leagues:\t\t\t",                  b2 <- length(unique(x$results[, leagueID])))
      cat("\nnumber of games:\t\t\t",                    b3 <- nrow(x$results), "\n\n")
      return(invisible(list(summary = X,
        nums = c(numTeams = a1, numTournaments = 0, numLeagues = b2, numGames = b3, numET = 0, numPen = 0))))
    }
  }
  return(invisible(list(summary = X,
    nums = c(numTeams = NA, numTournaments = NA, numLeagues = NA, numGames = NA, numET = NA, numPen = NA))))
}
