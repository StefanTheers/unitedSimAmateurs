#' Simulation of a tournament without fixed lineups, i.e. allowing for
#' permutations of the lineup matrix, plus league
#' @param lineups (N x 5)-dim. matrix of lineups where N=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param reps integer(3), named vector of the number of replications of a single tournament
#'   ("tournament"), of a single league ("league") and a single game ("game", only used for the league).
#'   Tournament games are NOT played multiple times.
#' @param ncores integer(1), number of processor cores for parallelization
#' @param sample logical(1), should a sample of possible tournament draws be used?
#'   If FALSE, then all possible draws are used (which might be computationally infeasible
#'   for more than 8 participating teams).
#' @param sample.size integer(1), the number of tournament draw samples drawn if sample=TRUE.
#' @param method character(1), either "expected" or "sample" (default)
#'   More details in game().
#' @param competition character(1), which competition should be played? Either "tournament",
#'   "league" or "both" (default).
#' @return list of simTournamentPerm() and simLeague() return values
#' @export
#'
#' @examples
#'
#' # define some lineups:
#' lineups <- cbind(t = c(4,0,0,4), a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5))
#' rownames(lineups) <- c("Nob", "FdS", "USV", "Marco")
#' lineups
#' lineups2 <- rbind(lineups, lineups)
#' rownames(lineups2) <- paste0(rownames(lineups), c(rep("", 4), rep(" II", 4)))
#' lineups2
#' lineups3 <- lineups[rep(1:4, 20)[1:64], ]
#' rownames(lineups3) <- paste(1:64, rownames(lineups3))
#' lineups3
#'
#' # Head to the examples section of SimTournamentPerm() for more info.
#' set.seed(2020)
#' (res1 <- simAmateurs(lineups, reps = c(tournament = 5, league = 20, game = 1),  ncores = 8, sample = FALSE, sample.size = 100, method = "expected"))
#' (res2 <- simAmateurs(lineups, reps = c(tournament = 5, league = 20, game = 1),  ncores = 8, sample = TRUE,  sample.size = 100, method = "expected"))
#' (res3 <- simAmateurs(lineups, reps = c(tournament = 5, league = 20, game = 1),  ncores = 8, sample = FALSE, sample.size = 100, method = "sample"))
#' (res4 <- simAmateurs(lineups, reps = c(tournament = 5, league = 20, game = 1),  ncores = 8, sample = TRUE,  sample.size = 100, method = "sample"))
#' (res5 <- simAmateurs(lineups, reps = c(tournament = 5, league = 20, game = 20), ncores = 8, sample = TRUE,  sample.size = 100, method = "sample"))
#' lapply(list(res1, res2, res3, res4, res5), "[[", "tournament")
#' lapply(list(res1, res2, res3, res4, res5), "[[", "league")
#'
#' # more realistic parameters for lineups matrices with 8 and 64 teams that yield good results:
#' \dontrun{
#' # 8 teams (3 minutes):
#' system.time(res8 <- simAmateurs(lineups2, reps = c(tournament = 10, league = 100, game = 20),
#'                                 ncores = 8, sample = TRUE, sample.size = 3000, method = "sample", competition = "both"))
#' res8      ##save(res8, file = "res8.rdata")
#' # 64 teams (tournament + league = 9 min + 3 min = 12 min):
#' system.time(res64 <- simAmateurs(lineups3, reps = c(tournament = 10, league = 100, game = 20),
#'                                  ncores = 8, sample = TRUE, sample.size = 3000, method = "sample", competition = "both"))
#' res64     ##save(res64, file = "res64.rdata")
#' }
#'
#'
simAmateurs <- function(lineups, reps = c(tournament = 100, league = 100, game = 1),
                        ncores = 8, sample = TRUE, sample.size = 100, method = "sample",
                        competition = "both") {

  if(competition == "both")
    competition <- c("tournament", "league")

  if("tournament" %in% competition) {
    cat("Simulation tournament ...\n")
    resTournament <- simTournamentPerm(
        lineups = lineups, reps = reps["tournament"],
        ncores = ncores, sample = sample, sample.size = sample.size, method = method)
    gc()
    cat("DONE\n\n")
  } else {
    resTournament <- NULL
  }

  if("league" %in% competition) {
    cat("Simulation league ...\n")
    resLeague <- simLeague(
        lineups = lineups, reps = reps["league"], ncores = ncores, method = method, repsGame = reps["game"])
    gc()
    cat("DONE\n\n")
  } else {
    resLeague <- NULL
  }

  return(list(tournament = resTournament, league = resLeague))
}