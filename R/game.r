#' Samples the number of goals scored in a game of x1 vx. x2.
#' @param x1 integer(5), named team vector of team 1
#' @param x2 integer(5), named team vector of team 2
#' @param etps logical(1), play etra time and penalty shoot-out? Default is TRUE.
#' @param method character(1), either "expected" or "sample" (default).
#'   If method="expected", use rounded (!) expected goals when determining
#'   the result of a game.
#'   This means that the better lineup will win.
#'   Examples for expected goals:
#'   0.4:0 (draw 0:0), 0.6:0.4 (x1 wins 1:0), 2.9:4.4 (x2 wins 3:4).
#'   If method="sample", sample the result.
#'   This means that the better lineup will not necessarily win.
#' @return integer(2), the number of goals scored by x1 and x2.
#'   The return value of class "game" has extra attributes:
#'   * fullTimeScore:   integer(2)
#'   * extraTimeScore:  integer(2)
#'   * penalties:       integer(2)
#' @note
#'   * Simulation and variable naming similar to a
#'     light version of playRound() in bfbunited for method="sample".
#'   * It is highly recommended to use method="sample", cf. examples.
#' @export
#'
#' @examples
#' library("data.table")
#' x1 <- c(t = 0,  a = 10, v = 16, m = 16, s = 48)
#' x2 <- c(t = 10, a = 10, v = 36, m = 12, s = 12)
#' set.seed(2020)
#' (res <- game(x1, x2))
#' str(res)
#' game(x1, x2)                       # default is method="sample"
#' game(x1, x2, method = "expected")  # deterministic
#'
#'
#' m <- 2
#' set.seed(2020)
#' replicate(m, game(   x1, x2, etps = TRUE, method = "sample"), simplify = FALSE)
#' set.seed(2020)
#' replicate(m, gameRep(x1, x2, etps = TRUE, method = "sample", repsGame = 1), simplify = FALSE)
#' # => The seed was set. Same results. That's good.
#'
#' set.seed(2020)
#' (res <- gameRep(x1, x2, etps = TRUE, method = "sample", repsGame = 5))
#' attr(res, "repScores")
#' # => These results seem to be correct.
#'
#' # winning ratio of x1 vs. x2:
#' mean(apply(replicate(1e4, game(x1, x2)), 2, function(x) x[1] > x[2]))
#' # winning ratio of x1 vs x2 (and x2 vs. x1) at full time:
#' mean(apply(tmp <- replicate(1e4, attr(game(x1, x2), "fullTimeScore")), 2,
#'            function(x) x[1] > x[2]))
#' mean(apply(tmp, 2, function(x) x[1] == x[2]))
#' mean(apply(tmp, 2, function(x) x[2] > x[1]))
#'
#' # comparison: The expected result is a 0:0 as 0.47 (0.00) goals are expected for x1 (x2):
#' c(expGoals(x1, x2), expGoals(x2, x1))
#' game(x1, x2, method = "expected")        # same as method="expected"
#'
#' # If we repeat the sampling approach sufficiently engough, do we converge to
#' # the expected result?
#' set.seed(2020)
#' res <- as.data.table(t(replicate(50, game(x1, x2, etps = FALSE, method = "sample"))))
#' res[, points1 := 3 * (V1 > V2) + (V1 == V2)]
#' res[, points2 := 3 * (V1 < V2) + (V1 == V2)]
#' res[, lapply(.SD, mean), .SDcols = points1:points2]   # much better
#' game(x1, x2, etps = FALSE, method = "expected")       # not much info
#' tmp <- res[, lapply(lapply(.SD, cumsum), function(x) x / 1:nrow(res)), .SDcols = points1:points2]
#' plot(0:1, type = "n", ylim = c(0,3), xlim = c(0, nrow(res)), ylab = "mean points per game", xlab = "number of games")
#' for(i in 1:2) lines(tmp[, ..i], col = i, lwd = 3)
#' abline(h = 1, col = 3, lty = 2, lwd = 3)
#' legend("topright", legend = c("team 1", "team 2", "team 1 + 2 in case of draw (method='expected'), rounded result"),
#'        col = 1:3, lwd = 3, bty = "n", cex = 0.9)
#' # No, we don't, which is not overly surprising. Therefore it is highly recommended
#' # NOT to use method="expected".
#' # => Replicate a game 20 times and use method="sample".
#'
game <- function(x1, x2, etps = TRUE, method = "sample") {

  checkmate::assertCharacter(method, pattern = "(expected)|(sample)")
  retVal <- integer(2)
  attr(retVal, "fullTimeScore")  <- integer(2)
  attr(retVal, "extraTimeScore") <- rep(NA, 2)
  attr(retVal, "penalties")      <- rep(NA, 2)
  class(retVal) <- "game"

  ##### expected results:
  if(method == "expected") {
    # full time:
    attr(retVal, "fullTimeScore") <- c(expGoals(x1, x2), expGoals(x2, x1))
    retVal[1:2] <- attr(retVal, "fullTimeScore")
    if(etps && retVal[1] == retVal[2]) {
      # extra time:
      attr(retVal, "extraTimeScore") <- 1/3 * attr(retVal, "fullTimeScore")
      retVal[1:2] <- retVal[1:2] + attr(retVal, "extraTimeScore")
      # penalty shout-out (sample winner if t1=t2):
      attr(retVal, "penalties") <- setNames(5 * c((1 - x2["t"] / 20), 1 - x1["t"] / 20), nm = NULL)
      if(attr(retVal, "penalties")[1] == attr(retVal, "penalties")[2]) {
        ind <- sample(1:2, size = 1)
        attr(retVal, "penalties")[ind] <- attr(retVal, "penalties")[ind] + 1
      }
      retVal[1:2] <- retVal[1:2] + attr(retVal, "penalties")
    }
  }

  ##### sample a game:
  if(method == "sample") {
    # full time:
    x1_chances <- (  max(0, round(x1["s"] - (x2["v"] + x2["a"])))
                   + max(0, round(1/2 * (x1["m"] - x2["m"])))
                   + max(0, round(1/4 * (x1["v"] - x2["s"]))))
    x1_efficiency <- (15 - x2["a"]) / 15 * (14 - x2["t"]) / 14
    x1_goals <- rbinom(1, size = x1_chances, prob = x1_efficiency)
    x2_chances <- (  max(0, round(x2["s"] - (x1["v"] + x1["a"])))
                   + max(0, round(1/2 * (x2["m"] - x1["m"])))
                   + max(0, round(1/4 * (x2["v"] - x1["s"]))))
    x2_efficiency <- (15 - x1["a"]) / 15 * (14 - x1["t"]) / 14
    x2_goals <- rbinom(1, size = x2_chances, prob = x2_efficiency)
    attr(retVal, "fullTimeScore") <- c(x1_goals, x2_goals)
    retVal[1:2] <- attr(retVal, "fullTimeScore")

    # extra time (30 instead of 90 mins, i.e. 1/3):
    if(etps && (x1_goals == x2_goals)) {
      x1_ET_goals <- rbinom(1, size = round(x1_chances / 3), prob = x1_efficiency)
      x2_ET_goals <- rbinom(1, size = round(x2_chances / 3), prob = x2_efficiency)
      attr(retVal, "extraTimeScore") <- c(x1_ET_goals, x2_ET_goals)
      retVal[1:2] <- retVal[1:2] + attr(retVal, "extraTimeScore")

      # penalty shoot-out:
      if(x1_ET_goals == x2_ET_goals) {
        if(x1["t"] == 0 && x2["t"] == 0) {
          x1["t"] <- x2["t"] <- 10            # very boring shoot-out elsewise
        }
        x1_PS_goals <- rbinom(1, size = 5, prob = 1 - x2["t"] / 20)
        x2_PS_goals <- rbinom(1, size = 5, prob = 1 - x1["t"] / 20)
        while(x1_PS_goals == x2_PS_goals) {
          x1_PS_goals <- rbinom(1, size = 1, prob = 1 - x2["t"] / 20)
          x2_PS_goals <- rbinom(1, size = 1, prob = 1 - x1["t"] / 20)
        }
        attr(retVal, "penalties") <- c(x1_PS_goals, x2_PS_goals)
        retVal[1:2] <- retVal[1:2] + attr(retVal, "penalties")
      }
    }
  }

  return(retVal)
}



#' Print method for game objects
#' @param x object of class "game"
#' @param extratime character(1), what to add for extra time result
#' @param penalty character(1), what to add for penalty shoot-out result
#' @param teamnames NULL or character(2), team names if known
#' @return invisible(character(1))
#' @export
print.game <- function(x, extratime = "n.V.", penalty = "i.E.", teamnames = NULL) {
  res <- paste(x, collapse = ":")
  if(!any(is.na(attr(x, "extraTimeScore")))) {
    res <- paste(paste(attr(x, "fullTimeScore") + attr(x, "extraTimeScore"), collapse = ":"), extratime)
    if(!any(is.na(attr(x, "penalties")))) {
      res <- paste0(res, " (", paste(attr(x, "penalties"), collapse = ":"), " ", penalty, ")")
    }
  }
  teams <- ifelse(is.null(teamnames), yes = "x1 vs. x2", no = paste(teamnames, collapse = " vs. "))
  res <- paste0(teams, ":\t", res, "\n")
  cat(res)
  return(invisible(res))
}



#' Internal helper function that calls game() repsGame-times
#' @param x1 integer(5), named team vector of team 1
#' @param x2 integer(5), named team vector of team 2
#' @param etps logical(1), play etra time and penalty shoot-out? Default is TRUE.
#' @param method character(1), either "expected" or "sample" (default).
#' @param repsGame integer(1), number of replications
#' @return integer(2) as in game()
#' @export
gameRep <- function(x1, x2, etps = TRUE, method = "sample", repsGame = 4) {
  res <- replicate(repsGame, game(x1, x2, etps = etps, method = method), simplify = FALSE)
  score    <- do.call(rbind, res)
  scoreFT  <- do.call(rbind, lapply(res, attr, "fullTimeScore"))
  scoreET  <- do.call(rbind, lapply(res, attr, "extraTimeScore"))
  scorePen <- do.call(rbind, lapply(res, attr, "penalties"))

  retVal <- colMeans(score)
  attr(retVal, "fullTimeScore")  <- colMeans(scoreFT)
  attr(retVal, "extraTimeScore") <- colMeans(scoreET,  na.rm = TRUE)
  attr(retVal, "penalties")      <- colMeans(scorePen, na.rm = TRUE)
  attr(retVal, "repScores")      <-
    data.table(score1    = score[, 1],    score2    = score[, 2],
               scoreFT1  = scoreFT[, 1],  scoreFT2  = scoreFT[, 2],
               scoreET1  = scoreET[, 1],  scoreET2  = scoreET[, 2],
               scorePen1 = scorePen[, 1], scorePen2 = scorePen[, 2])
  class(retVal) <- "game"

  # If the mean full time scoreline defines a winner, we want this team to
  # be the winnner. (Do we? It's better not to use repetitions for tournaments ...)
  if(length(unique(attr(retVal, "fullTimeScore"))) > 1) {
    retVal[1:2] <- attr(retVal, "fullTimeScore")
  }

  return(retVal)
}
