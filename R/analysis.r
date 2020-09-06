#' Analysis of a simulation result of a tournament and league
#'
#' Analyse a simulation result returned by \code{\link{simAmateurs}}.
#'
#' @param x [object of S3 class \code{"simResult"}]\cr
#'   list with \code{tournament} and \code{league} entries, each of class \code{simResult}
#' @param truth [\code{data.frame}]\cr
#'   with true results with two columns \code{team}
#'   and \code{tournamentRoundResult} (points won in the tournament, 1 point per game that was won)
#' @return \code{list} with results of class \code{simResultAnalysis}
#' @seealso Call \code{\link{simAmateurs}} to create an object of class \code{"simResult"}.
#' Use \code{\link{plot.simResultAnalysis}} for a visualization of the analysis result and
#' \code{\link{mergeAnalysis}} to merge two or more analysis results.
#' \code{\link{print.simResultAnalysis}} basically just calls \code{\link[base]{print.default}}
#' but omits to print the \code{x} attribute (the input of class \code{"simResult"}).
#' @export
#'
#' @examples
#' # define some lineups:
#' lineups <- cbind(t = c(4,0,0,4), a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5))
#' rownames(lineups) <- c("Nob", "FdS", "USV", "Marco")
#' lineups
#' lineups2 <- cbind(t = c(4,0,0,4) + 6, a = c(0,4,4,0), v = c(5,5,8,8), m = c(8,5,5,5), s = c(5,8,5,5) + 1)
#' rownames(lineups2) <- c("Nob", "FdS", "USV", "Marco")
#' lineups2
#'
#'
#' # run the simulation:
#' set.seed(2020)
#' (res1 <- simAmateurs(lineups, reps = c(tournament = 5, league = 20, game = 1), ncores = 8, sample = TRUE,  sample.size = 100, method = "sample"))
#' (res2 <- simAmateurs(lineups2, reps = c(tournament = 5, league = 20, game = 1), ncores = 8, sample = TRUE,  sample.size = 100, method = "sample"))
#'
#' # define the true ("official") tournament results:
#' (truth1 <- data.table(team = rownames(lineups),  points = c(0,0,1,2))) # Marco winner, USV 2nd finalist
#' (truth2 <- data.table(team = rownames(lineups2), points = c(0,0,2,1))) # vice versa
#'
#' # analyse the simulation result:
#' (ana1 <- analysis(res1, truth1))
#' (ana2 <- analysis(res2, truth2))
#' # Let's look at ana2:
#' #
#' # $summary contains the following team-based columns:
#' # * bfb is the true result from the truth input.
#' #   Note: DFB is the German Football Association (Deutscher Fussball Bund),
#' #         BFB is its limited United counterpart (Beschraenkter Fussball Bund).
#' # * pointsAvg: The most important column that contains the simulation results:
#' #   The average number of points per tournament.
#' # * wonNumber: The number of tournaments won by each team.
#' # * wonProp: The proportion of tournaments won by each team.
#' # * pointsAvgRank: The ranking based on pointsAvg.
#' # * wonPropRank: The ranking based on wonProp.
#' # * 0,1,2, ...: The discrete distribution of points won per tournament for each team.
#' # * worse:  0.00  means that FdS have never had less points in the simulation in comparison to the official bfb result.
#' # * same:   0.502 means that FdS have had the same number of points in 50.2% of all simulated tournaments as in the
#' #           official bfb result.
#' # * better: 0.498 means that FdS have been better in 49.8% of all simulated tournaments than in the official bfb result.
#' # * leaguePointsAvg: The average number of league points.
#' # * leagueWonNumber: The number of leagues won by each team.
#' # * leagueWonProp: The proportion of leagues won by each team.
#' # * leaguePointsAvgRank: The ranking based on leaguePointsAvg.
#' # * leagueWonPropRank: The ranking based on leagueWonProp.
#' # * tournamentLeagueRankDiff: pointsAvgRank - leaguePointsAvgRank
#' # * leaguePointsMin: The minimum (i.e. worst) number of points per league.
#' # * leaguePointsMax: The maximum (i.e. best)  number of points per league.
#' # * leagueRankWorst: The worst league position that occured for each team.
#' # * leagueRankBest:  The best  league position that occured for each team.
#' #
#' # $notTeamBased contains a vector with min and max values of interest of the same, better and worse columns
#' # and a matrix of Spearman rank correlations.
#' #
#' # What do we actually see?
#' # * An official tournament winner USV is pretty unfair: USV have performed worse in 94% of all simulated tournaments
#' #   and have never won a league. In fact, it is clearly visible in the pointsAvg and leaguePointsAvg columns
#' #   that USV is by far the worst team in both simulated competitions (tournament and league).
#' # * This unfairness is also displayed in the correlation matrix (cf. bfb results).
#' # * Nob is the simulated tournament and FdS the simulated league winner.
#'
#' # plot of the discrete distribution of the simulated tournament performance of each team:
#' plot(ana1, type = "distrib", main = "p=26 WP")
#' plot(ana2, type = "distrib", main = "p=39 WP")
#' # Let's look at ana1:
#' # * FdS and USV usually come in last (0 points).
#' # * Marco has a very even distribution with similar probability mass on 0,1 and 2 points.
#' # * Nob wins more than 50% of the simulated tournaments (2 points = the max number of points per tournament).
#'
#' # scatter plot with a comparison of the true result and the tournament simulation results:
#' plot(ana1, type = "scatter", main = "p=26 WP")
#' plot(ana2, type = "scatter", main = "p=39 WP")
#' # Let's look at ana1:
#' # * FdS and especially Nob are above of the bisecting line, i.e. they were unlucky in the official
#' #   BFB tournament (better performance in the simulation).
#' # * On the other hand, USV and Marco were lucky and performed better in the official tournament
#' #   than expected.
#'
#' # It is possible to analyse two or more simulation results:
#' (anaMerge <- mergeAnalysis(x = list(ana1, ana2)))
#' plot(anaMerge, type = "distrib", main = "merged results for p=81,71,75 WP")
#' plot(anaMerge, type = "scatter", main = "merged results for p=81,71,75 WP")
#' # (No bisecting line as the bfb points are added!)
#'
analysis <- function(x, truth) {

  ##### preparations
  colnames(truth) <- c("team", "bfb")
  if(class(x$tournament) != "simResult" | class(x$league) != "simResult")
    stop("Both x$tournament and x$league must be simResult objects!")


  ##### team-based statistics
  X <- merge(x = truth, y = x$tournament$summary, by = "team")
  X[, pointsAvgRank := nrow(X) - rank(pointsAvg, ties.method = "min") + 1]
  X[, wonPropRank   := nrow(X) - rank(wonProp,   ties.method = "min") + 1]

  # Xpoints counts the number of games won by each team for each tournament.
  # (Note: If a team is not mentioned for a tournamentID, it lost in stage 1 and got 0 points).
  numTournaments <- x$tournament$results[, length(unique(tournamentID))]
  Xpoints <- x$tournament$results[, .N, by = list(tournamentID, winner)]
  Xpoints[, Nfactor := factor(N, levels = 0:max(N))]

  # What is the discrete distribution of the points for each team?
  Xdistrib <- Xpoints[, table(Nfactor), by = winner]
  a <- unique(Xdistrib[, table(winner)])
  if(length(a) != 1)
    stop("Internal error: Please report to the package author!")
  Xdistrib <- split(Xdistrib, f = Xdistrib[, winner])
  for(i in seq_along(Xdistrib))
    Xdistrib[[i]][, points := 0:(a-1)]
  Xdistrib <- do.call(rbind, lapply(Xdistrib, function(xx) setNames(xx[, V1], xx[, points])))
  Xdistrib[, "0"] <-  numTournaments - rowSums(Xdistrib)
  Xdistrib <- data.table(team = rownames(Xdistrib), as.data.frame(Xdistrib))
  X <- merge(X, Xdistrib, by = "team")

  # The teams were better/ worse in how many tournaments (in comparison to the BFB result)?
  # (Note: If better has a high value, then a team has been very unlucky in the true BFB tournament).
  ind <- colnames(X)[grep("^[0-9]{1-3}$", colnames(X))]
  indMax <- as.numeric(rev(ind)[1])
  X[, worse  := 0]
  X[, same   := 0]
  X[, better := 0]
  for(i in 1:nrow(X)) {
    if(X[i, bfb] > 0)
      X[i, worse  :=  rowSums(X[i, as.character(0:max(0, X[i, bfb] - 1)),           with = FALSE]) / numTournaments]
    X[  i, same   :=  rowSums(X[i, as.character(X[i, bfb]), with = FALSE])                         / numTournaments]
    if(X[i, bfb] < indMax)
      X[i, better :=  rowSums(X[i, as.character(min(X[i, bfb] + 1, indMax):indMax), with = FALSE]) / numTournaments]
  }

  # Compare the mean tournament result to the mean league result:
  Xleague <- x$league$summary
  setnames(Xleague, old = c("pointsAvg",       "wonNumber",       "wonProp"),
                    new = c("leaguePointsAvg", "leagueWonNumber", "leagueWonProp"), skip_absent = TRUE)
  X <- merge(X, Xleague, by = "team")
  X[, leaguePointsAvgRank := nrow(X) - rank(leaguePointsAvg, ties.method = "min") + 1]
  X[, leagueWonPropRank   := nrow(X) - rank(leagueWonProp,   ties.method = "min") + 1]
  X[, tournamentLeagueRankDiff := pointsAvgRank - leaguePointsAvgRank]

  Xleague2 <- x$league$results
  Xleague2[, points1 := 3 * (score1 > score2) + (score1 == score2)]
  Xleague2[, points2 := 3 * (score1 < score2) + (score1 == score2)]
  Xleague3 <- rbind(data.table(team = Xleague2[, team1], points = Xleague2[, points1], leagueID = Xleague2[, leagueID]),
                    data.table(team = Xleague2[, team2], points = Xleague2[, points2], leagueID = Xleague2[, leagueID]))
  Xleague3 <- Xleague3[, sum(points), by = list(leagueID, team)]
  setnames(Xleague3, old = "V1", new = "points", skip_absent = TRUE)
  ## Xleague3[, mean(points), by = team] # for comparison with Xleague
  Xmin <- Xleague3[, min(points), by = team]
  Xmax <- Xleague3[, max(points), by = team]
  setnames(Xmin, old = "V1", new = "leaguePointsMin", skip_absent = TRUE)
  setnames(Xmax, old = "V1", new = "leaguePointsMax", skip_absent = TRUE)
  Xleague4 <- merge(Xmin, Xmax, by = "team")
  # look at the ranks:
  XleagueList <- split(Xleague3, f = Xleague3[, leagueID])
  for(i in seq_along(XleagueList))
    XleagueList[[i]][, rank := nrow(X) - rank(points, ties.method = "min") + 1]
  Xleague3b <- do.call(rbind, XleagueList)
  XminRank <- Xleague3b[, min(rank), by = team]
  XmaxRank <- Xleague3b[, max(rank), by = team]
  setnames(XminRank, old = "V1", new = "leagueRankBest")
  setnames(XmaxRank, old = "V1", new = "leagueRankWorst")
  Xleague4b <- merge(XmaxRank, XminRank,  by = "team", skip_absent = TRUE)
  Xleague5  <- merge(Xleague4, Xleague4b, by = "team", skip_absent = TRUE)
  X <- merge(X, Xleague5, by = "team")


  ##### not team-based statistics
  Y <- list()
  Y[["bfbTournamentSimComparison"]] <-
    c(sameMin   = X[, min(same)],
      sameMean  = X[, mean(same)],
      sameMax   = X[, max(same)],
      betterMax = X[, max(better)],
      worseMax  = X[, max(worse)])
  Y[["corRankings"]] <- cor(X[, list(bfb, pointsAvg, leaguePointsAvg)], method = "spearman")


  retVal <- list(summary = X, notTeamBased = Y)
  class(retVal) <- "simResultAnalysis"
  return(retVal)
}





#' Visualize the analysis results of a simulation
#' @param x object of class \code{simResultAnalysis}
#' @param type [\code{character(1)}]\cr
#'   plot type with
#'   \code{type="distrib"} for a plot of the discrete distribution of the simulated
#'   tournament performance of each team or
#'   \code{type="scatter"} for a scatter plot with a comparison of the true result
#'   and the tournament simulation results
#' @param ... further arguments that are passed to the \code{plot} functions
#' @return \code{invisible(NULL)}
#' @seealso \code{\link{analysis}}
#' @export
#'
plot.simResultAnalysis <- function(x, type = "distrib", ...) {
  X <- x$summary
  if(type == "distrib") {
    # plot of the discrete distributions of each team from Xdistrib:
    ind <- colnames(X)[grep("^[0-9]{1-3}$", colnames(X))]
    dat <- as.data.frame(X[, ..ind])
    rownames(dat) <- X$team
    dat <- dat / rowSums(dat)[1]
    plot(0:1, type = "n", xlim = c(0, ncol(dat)), ylim = c(0, max(dat[, ind[-1]])),
      xlab = "points in 1 tournament", ylab = "relative frequency", ...)
    grid()
    mycols <- rainbow(nrow(dat))
    for(i in 1:nrow(dat)) {
      lines(x = 0:(ncol(dat) - 1), y = dat[i, ], type = "b", col = mycols[i], pch = 20)
      text(x = ncol(dat) - 1, y = rev(dat[i, ])[1], labels = rownames(dat)[i],
        col = mycols[i], cex = 0.85, pos = 4)
    }
  }
  if(type == "scatter") {
    # comparison of BFB result and tournament simulation results:
    plot(X[, bfb], X[, pointsAvg], type = "n",
      xlim = c(0, X[, max(bfb) + 0.9]), ylim = c(0, X[, max(pointsAvg)]),
      xlab = "points of official BFB result", ylab = "average points in 1 tournament (simulation)", ...)
    grid()
    if(length(attributes(x)) <= 2)  # no abline for merged analysis objects (does not make sense!)
      abline(a = 0, b = 1, col = "gray", lty = 2)
    mycols <- rainbow(nrow(x$summary))
    points(X[, bfb], X[, pointsAvg], col = mycols, pch = 20)
    text(X[, bfb], X[, pointsAvg], labels = X[, team], cex = 0.85, col = mycols, pos = 4)
  }
  return(invisible(NULL))
}





#' Merge a list of \code{simResultAnalysis} objects
#'
#' @param x [\code{list}]\cr
#'   list of \code{simResultAnalysis} objects
#' @return \code{list} with results of class \code{"simResultAnalysis"}
#' @seealso \code{\link{analysis}}
#' @export
mergeAnalysis <- function(x) {

  k <- length(x)

  ##### team-based statistics
  Xlist <- lapply(x, "[[", "summary")

  # basic checks:
  M <- do.call(cbind, lapply(Xlist, colnames))
  if(!all(M[, 1] == M))
    stop("All elements in x must have the same columns (in the same order)!")
  M <- do.call(cbind, lapply(Xlist, "[[", "team"))
  if(!all(M[, 1] == M))
    stop("All teams columns in x must be identical (i.e. in the same order)!")

  # init X that summarizes all $summary entries contained in the x input:
  X <- Xlist[[1]][, .(team)]

  # columns in colsMean are summarized by calculating their mean.
  # columns in colsSum  are summarized by adding them to each other.
  colsDistrib <- colnames(Xlist[[1]])[grep("^[0-9]{1-3}$", colnames(Xlist[[1]]))]
  colsMean <- c("pointsAvg", "wonProp", "pointsAvgRank", "wonPropRank",
                "leaguePointsAvg", "leagueWonProp", "leaguePointsAvgRank", "leagueWonPropRank")
  colsSum  <- c("bfb", "wonNumber", colsDistrib, "leagueWonNumber")
  colsMin  <- c("leaguePointsMin", "leagueRankWorst")
  colsMax  <- c("leaguePointsMax", "leagueRankBest")
  for(j in colsMean)
    X[, j] <- rowMeans(do.call(cbind, lapply(Xlist, "[[", j)))
  for(j in colsSum)
    X[, j] <- rowSums(do.call(cbind, lapply(Xlist, "[[", j)))
  for(j in colsMin)
    X[, j] <- apply(do.call(cbind, lapply(Xlist, "[[", j)), 1, min)
  for(j in colsMax)
    X[, j] <- apply(do.call(cbind, lapply(Xlist, "[[", j)), 1, max)
  X[, tournamentLeagueRankDiff := pointsAvgRank - leaguePointsAvgRank]
  # bfb is now the sum of all bfb points. So this definition of worse/same/better
  # does not make sense anymore and is hence set to NA:
  X[, worse  := NA]
  X[, same   := NA]
  X[, better := NA]
  setcolorder(X, colnames(Xlist[[1]]))


  ##### not team-based statistics
  Y <- list()
  Y[["bfbTournamentSimComparison"]] <-
    c(sameMin   = X[, min(same)],
      sameMean  = X[, mean(same)],
      sameMax   = X[, max(same)],
      betterMax = X[, max(better)],
      worseMax  = X[, max(worse)])
  Y[["corRankings"]] <- cor(X[, list(bfb, pointsAvg, leaguePointsAvg)], method = "spearman")


  # cat some info:
  cat("\nMerge", k, "simResultAnalysis objects:",
      "\n\nThe following columns are summarized by calculating their mean:",
      paste(c("", colsMean), collapse = "\n\t* "),
      "\n\nThe following columns are summarized by adding them to each other:",
      paste(c("", colsSum), collapse = "\n\t* "),
      "\n\nThe following columns are summarized by calculating their minimum:",
      paste(c("", colsMin), collapse = "\n\t* "),
      "\n\nThe following columns are summarized by calculating their maximum:",
      paste(c("", colsMax), collapse = "\n\t* "),
      "\n\nNote: worse, same and better are set to NA as bfb is now the sum of all bfb points.",
      "\n\nDONE\n\n")

  retVal <- list(summary = X, notTeamBased = Y)
  class(retVal) <- "simResultAnalysis"
  attr(retVal, "x") <- x
  return(retVal)
}




#' \code{print} method for \code{"simResultAnalysis"} objects
#'
#' This print method basically just calls \code{\link[base]{print.default}}
#' but omits to print the \code{x} attribute (the input of class \code{"simResult"}).
#'
#' @param x [object of S3 class \code{"simResultAnalysis"}]\cr
#'   the return value of either \code{\link{analysis}} or \code{\link{mergeAnalysis}}
#' @param ... further arguments passed to \code{\link[base]{print.default}}
#' @return \code{invisible(x)}
#' @export
print.simResultAnalysis <- function(x, ...) {
  if(length(attributes(x)) > 2)
    attr(x, "x") <- NULL
  print.default(x, ...)
}
