#' Simulation of a tournament without fixed lineups, i.e. allowing for
#' permutations of the lineup matrix
#' @param lineups (N x 5)-dim. matrix of lineups where N=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param reps integer(1), number of replications of single tournament
#' @param ncores integer(1), number of processor cores for parallelization
#' @note Note that (A:B, C:D) and (D:C, B:A) etc. are the same tournament!
#' @param sample logical(1), should a sample of possible tournament draws be used?
#'   If FALSE, then all possible draws are used (which might be computationally infeasible
#'   for more than 8 participating teams).
#' @param sample.size integer(1), the number of tournament draw samples drawn if sample=TRUE.
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
#' lineups2 <- rbind(lineups, lineups)
#' rownames(lineups2) <- paste0(rownames(lineups), c(rep("", 4), rep(" II", 4)))
#' lineups2
#'
#' # run the simulation (with all possible lineups permutations):
#' set.seed(2020)
#' res <- simTournamentPerm(lineups, reps = 3, ncores = 8, sample = FALSE, sample.size = 100, method = "sample")
#' res
#' names(res)
#'
#' res$results
#' # These are the results of all games that were played in all tournaments.
#' # tournamentID = (#lineup permutation - #repetition of this very same tournament)
#' # FT = full time, ET = extra time, Pen = penalty shoot-out
#'
#' res$summary
#' # The main simulation results with estimators for:
#' # * pointsAvg:
#' #   number of points awarded to the teams in 1 tournament:
#' #   1 point = 1 game won. For 4 teams, each team has
#' #   played 2 games, i.e. 3 points are awarded in total and the
#' #   tournament winner gets 2, the losing finalist 1, the other two 0.
#' # * wonNumber:
#' #   total number of tournaments that were won, i.e. in [0, reps * sample.size].
#' # * wonProp:
#' #   wonNumber / (reps * number of permutations), i.e. proportion of tournaments won.
#' #   If sample=TRUE,  number of permutations = sample.size.
#' #   If sample=FALSE, number of permutations = factorial(number of teams).
#'
#' res$lineups
#' # the team's lineups
#'
#' res$resultsTournament[1:2]
#' # list of reps-times repeated tournaments for the first two lineups' permutations
#'
#' res$summary[, sum(pointsAvg)]   # = number of points awarded in each tournament
#' res$summary[, sum(wonNumber)]   # = reps * number of permutations = number of tournaments played
#' res$summary[, sum(wonProp)]     # = 1 = 100%
#'
#' res
#' # The print method is called here.
#' # Nob is the overall best team with 1.31 points per tournament.
#' # Nob also won 63% of all tournaments that were played.
#'
#'
#' # Run the simulation again (with 100 sampled lineups permuations):
#' set.seed(2020)
#' system.time(res <- simTournamentPerm(lineups, reps = 3, ncores = 8, sample = TRUE, sample.size = 100, method = "sample"))
#' res
#'
#'
#' # Increasing the number of teams (4 to 8) would make the simulation much slower if
#' # sample=FALSE was chosen:
#' set.seed(2020)
#' system.time(res <- simTournamentPerm(lineups2, reps = 3, ncores = 8, sample = TRUE, sample.size = 100, method = "sample"))
#' res
#'
#'
#' # Another look at the sample and methods inputs:
#' # * 4 teams participate in the tournaments.
#' # * factorial(4) possible lineups permutations (i.e. different tournaments) are possible.
#' # * Every tournament is played 5 times (if method="sample") or once (if method="expected").
#' # * As there are 4 teams participating in each tournament, 2 semi-finals plus 1 final are
#' #   played. Alltogether that's 3 games per tournament to determine the winner.
#' # * If method="sample", we sample 100 row permutations of the lineups matrix out of
#' #   factorial(4) possible permutations (with repetition).
#' set.seed(2020)
#' (res <- simTournamentPerm(lineups, reps = 5, ncores = 8, sample = FALSE, sample.size = 100, method = "expected"))
#' length(unique(res$results[, tournamentID])) # =     factorial(4) tournaments
#' nrow(res$results)                           # = 3 * factorial(4) games
#' (res <- simTournamentPerm(lineups, reps = 5, ncores = 8, sample = TRUE, sample.size = 100, method = "expected"))
#' length(unique(res$results[, tournamentID])) # =     100 tournaments
#' nrow(res$results)                           # = 3 * 100 games
#' (res <- simTournamentPerm(lineups, reps = 5, ncores = 8, sample = FALSE, sample.size = 100, method = "sample"))
#' length(unique(res$results[, tournamentID])) # =     5 * 24 tournaments
#' nrow(res$results)                           # = 3 * 5 * 24 games
#' (res <- simTournamentPerm(lineups, reps = 5, ncores = 8, sample = TRUE, sample.size = 100, method = "sample"))
#' length(unique(res$results[, tournamentID])) # =     5 * 100  tournaments
#' nrow(res$results)                           # = 3 * 5 * 1500 games
#'
#'
simTournamentPerm <- function(lineups, reps = 100, ncores = 8, sample = TRUE, sample.size = 100, method = "sample", repsGame = 1) {
  checkmate::assertCharacter(method, pattern = "(expected)|(sample)")

  # permutation of lineups:
  numTeams <- nrow(lineups)
  perms <- drawPerm(n = numTeams, sample = sample, sample.size = sample.size)
  perms <- split(perms, rep(1:ncol(perms), each = nrow(perms)))
  numPerms <- length(perms)

  # result list: 1 entry for 1 tournament (and its reps replications):
  cl <- makeCluster(rep("localhost", ncores), type = "SOCK")
  clusterSetRNGStream(cl)
  clusterExport(cl, list("perms", "simTournament", "reps", "lineups", "method", "repsGame",
                         "tournament", "game", "points.tournament",
                         "as.data.table.tournament", "data.table", ":=",
                         "as.data.table", "expGoals"),
                envir = environment())
  res <- parLapply(cl, perms, function(ind) {
    simTournament(lineups = lineups[ind, ], reps = reps, method = method, repsGame = repsGame)
  })
  stopCluster(cl)

  # full results:
  X <- lapply(res, "[[", "results")
  for(i in seq_along(X))
    X[[i]][, tournamentID := paste(i, tournamentID, sep = "-")]
  resResults <- do.call(rbind, X)

  # full results with tournament objects:
  resResultsTournament <- lapply(res, "[[", "resultsTournament")

  # summary/ points:
  X <- lapply(res, "[[", "summary")
  X <- do.call(rbind, X)
  resSummary <- X[, lapply(.SD, sum), by = team, .SDcols = pointsAvg:wonProp]
  resSummary[, pointsAvg := pointsAvg / numPerms]   # mean instead of sum
  resSummary[, wonProp   := wonProp   / numPerms]   # mean instead of sum

  retVal <- list(results = resResults, summary = resSummary, lineups = lineups,
                 resultsTournament = resResultsTournament)
  class(retVal) <- "simResult"
  return(retVal)
}


