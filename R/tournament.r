#' Play tournament.
#' @param lineups (N x 5)-dim. matrix of lineups where N=2,4,8,16, ...
#'   Column names are t,a,v,m,s.
#'   Row names are the teams.
#' @param method character(1), either "expected" or "sample" (default).
#'   More details in game().
#' @param repsGame integer(1), number of repetitions of each game (if method="sample").
#'   Default is 1 (i.e. no repetitions). repsGame > 1 is NOT recommended for tournaments.
#' @return list with results of class "tournament"
#' @note
#' * The row-wise order of lineups determines the tournament:
#'   stage 1:
#'     row1 vs. row2
#'     row3 vs. row4
#'     ...
#'   stage 2:
#'     winner1 vs. winner2
#'     winner3 vs. winner4
#'     ...
#'   As many stages follow as necessary to determine an overall
#'   winner in a final stage, the final.
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
#' set.seed(2020)
#' res <- tournament(lineups)
#' str(res)
#' points(res)
#' as.data.table(res)
#' res
#'
#' tournament(lineups)
#' tournament(lineups)                       # different from 1st two runs
#' tournament(lineups, method = "expected")  # always the same
#'
#' tournament(lineups2)
#'
#' # For tournaments, it's really not recommended to use repsGame > 1:
#' set.seed(2020)
#' tournament(lineups)
#' tournament(lineups)
#' set.seed(2020)
#' (res <- tournament(lineups, repsGame = 5))
#' as.data.table(res)
#' # => Here, we we actually have decisions without extra time.
#'
tournament <- function(lineups, method = "sample", repsGame = 1) {
  N <- nrow(lineups)   # number of participating teams
  A <- lineups         # active lineups still participating in tournament
  retVal <- list()     # result list. 1 element for each tournament stage
  s <- 1               # stage counter

  if(!checkmate::testIntegerish(log(N, 2)))
    stop("For a tournament, N=2^k teams are necessary. Here, N=", N, "!")

  # Continue as long as there are at least 2 teams participating in
  # the tournament:
  while(is.matrix(A)) {
    stageRes <- list()
    n <- nrow(A) # number of participating teams in this stage
    g <- 1       # index for games during a stage
    i <- 1       # index for teams
    while(i < n) {
      if(repsGame == 1) {
        stageRes[[g]] <- game(A[i, ], A[i+1, ], method = method)
      } else {
        stageRes[[g]] <- gameRep(A[i, ], A[i+1, ], method = method, repsGame = repsGame)
      }
      names(stageRes[[g]]) <- rownames(A[c(i, i+1), ])
      attr(stageRes[[g]], "winner") <- names(which.max(stageRes[[g]]))
      g <- g + 1
      i <- i + 2
    }
    winners <- do.call(c, lapply(stageRes, function(x) attr(x, "winner")))
    retVal[[s]] <- stageRes
    A <- A[winners, ]
    s <- s + 1
  }

  # name the stages:
  S <- length(retVal)   # number of stages played
  nam <- paste("last", 2^(S:1))
  nam <- gsub("last 2", "final", nam)
  nam <- gsub("last 4", "semi-final", nam)
  names(retVal) <- nam

  class(retVal) <- "tournament"
  attr(retVal, "lineups") <- lineups
  return(retVal)
}



#' Award points to teams participating in a tournament
#' @param x list of class "tournament"
#' @return integer(1), vector of points awarded to each team.
#'   1 point is awarded for each win.
points.tournament <- function(x) {
  points <- integer(nrow(attr(x, "lineups")))
  names(points) <- rownames(attr(x, "lineups"))
  for(s in seq_along(x)) {
    winners <- do.call(c, lapply(x[[s]], function(xx) attr(xx, "winner")))
    points[winners] <- points[winners] + 1
  }
  return(points)
}



#' Print method for tournament objects
#' @param x list of class "tournament"
#' @return invisible(x)
print.tournament <- function(x) {
  nam <- names(x)
  cat("\n")
  for(s in seq_along(x)) {
    cat(paste0(rep("-", 40), collapse = ""),
        paste0("\n ", nam[s], ":"),
        paste0("\n", paste0(rep("-", 40), collapse = "")), "\n")
    lapply(x[[s]], function(xx) print(xx, teamnames = attr(xx, "names")))
    cat("\n")
  }
  cat("\nThis leads to points awarded as follows:\n\n")
  print(sort(points(x), decreasing = TRUE))
  cat("\n")
  return(invisible(x))
}



#' Convert a tournament object into a data.table
#' @param x list of class "tournament"
#' @return invisible(x)
as.data.table.tournament <- function(x) {
  XX <- lapply(x, function(xx) {
    XXX <- lapply(xx, function(xxx) {
      data.table(
          team1  = attr(xxx, "names")[1],
          team2  = attr(xxx, "names")[2],
          score1 = xxx[1],
          score2 = xxx[2],
          scoreFT1 = attr(xxx, "fullTimeScore")[1],
          scoreFT2 = attr(xxx, "fullTimeScore")[2],
          scoreET1 = attr(xxx, "extraTimeScore")[1],
          scoreET2 = attr(xxx, "extraTimeScore")[2],
          scorePen1 = attr(xxx, "penalties")[1],
          scorePen2 = attr(xxx, "penalties")[2],
          winner    = attr(xxx, "winner")
          )
    })
    do.call(rbind, XXX)
  })
  for(i in seq_along(XX))
    XX[[i]][, stage := names(XX)[i]]
  retVal <- do.call(rbind, XX)
  return(retVal)
}

