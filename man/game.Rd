% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/game.r
\name{game}
\alias{game}
\title{Play a game \code{x1} vs. \code{x2}}
\usage{
game(x1, x2, etps = TRUE, method = "sample")
}
\arguments{
\item{x1}{[\code{integer(5)}]\cr
named team vector of team 1}

\item{x2}{[\code{integer(5)}]\cr
named team vector of team 2}

\item{etps}{[\code{logical(1)}]\cr
Play extra time and penalty shoot-out? Default is \code{TRUE}.}

\item{method}{[\code{character(1)}]\cr
Either \code{"expected"} or \code{"sample"} (default).
If \code{method="expected"}, use expected goals when determining
the result of a game.
This means that the better lineup will win. \cr
If \code{method="sample"}, sample the result.
This means that the better lineup will not necessarily win, but
is has a higher probability to do so.}
}
\value{
object of S3 class \code{game} consisting of \code{integer(2)}, the overall number of goals
  scored by \code{x1} and \code{x2} with extra attributes:
  \describe{
    \item{\code{fullTimeScore}}{  [\code{integer(2)}]\cr full-time scoreline }
    \item{\code{extraTimeScore}}{ [\code{integer(2)}]\cr extra-time result }
    \item{\code{penalties}}{      [\code{integer(2)}]\cr penalty shoot-out result }
  }
}
\description{
Either samples the number of goals scored in a game of \code{x1} vs. \code{x2}
(\code{method="sample"}) or returns the number of expected goals of each team
(\code{method="expected"}).
}
\note{
\itemize{
    \item Code structure and variable naming are very similar to a
      light version of \code{playRound} in
      \href{https://gitlab.com/fchalligalli/bfbunited}{\code{bfbunited}} for \code{method="sample"}.
    \item It is highly recommended to use \code{method="sample"}, cf. examples.
    \item Rounding the results would yield:
      0.4:0 (draw 0:0), 0.6:0.4 (\code{x1} wins 1:0), 2.9:4.4 (\code{x2} wins 3:4).
  }
}
\examples{
x1 <- c(t = 0,  a = 10, v = 16, m = 16, s = 48)
x2 <- c(t = 10, a = 10, v = 36, m = 12, s = 12)
set.seed(2020)
(res <- game(x1, x2))
str(res)
game(x1, x2)                       # default is method="sample"
game(x1, x2, method = "expected")  # deterministic


m <- 2
set.seed(2020)
replicate(m, game(   x1, x2, etps = TRUE, method = "sample"), simplify = FALSE)
set.seed(2020)
replicate(m, gameRep(x1, x2, etps = TRUE, method = "sample", repsGame = 1), simplify = FALSE)
# => The seed was set. Same results. That's good.

set.seed(2020)
(res <- gameRep(x1, x2, etps = TRUE, method = "sample", repsGame = 5))
attr(res, "repScores")
# => These results seem to be correct.

# winning ratio of x1 vs. x2:
mean(apply(replicate(1e4, game(x1, x2)), 2, function(x) x[1] > x[2]))
# winning ratio of x1 vs x2 (and x2 vs. x1) at full time:
mean(apply(tmp <- replicate(1e4, attr(game(x1, x2), "fullTimeScore")), 2,
           function(x) x[1] > x[2]))
mean(apply(tmp, 2, function(x) x[1] == x[2]))
mean(apply(tmp, 2, function(x) x[2] > x[1]))

# comparison: The expected result is a 0:0 as 0.47 (0.00) goals are expected for x1 (x2):
c(expGoals(x1, x2), expGoals(x2, x1))
game(x1, x2, method = "expected")        # same as method="expected"

# If we repeat the sampling approach sufficiently often, do we converge to
# the expected result?
set.seed(2020)
res <- as.data.table(t(replicate(50, game(x1, x2, etps = FALSE, method = "sample"))))
res[, points1 := 3 * (V1 > V2) + (V1 == V2)]
res[, points2 := 3 * (V1 < V2) + (V1 == V2)]
res[, lapply(.SD, mean), .SDcols = points1:points2]   # much better
game(x1, x2, etps = FALSE, method = "expected")       # not much info
tmp <- res[, lapply(lapply(.SD, cumsum), function(x) x / 1:nrow(res)), .SDcols = points1:points2]
plot(0:1, type = "n", ylim = c(0,3), xlim = c(0, nrow(res)), ylab = "mean points per game", xlab = "number of games")
for(i in 1:2) lines(tmp[, ..i], col = i, lwd = 3)
abline(h = 1, col = 3, lty = 2, lwd = 3)
legend("topright", legend = c("team 1", "team 2", "team 1 + 2 in case of draw (method='expected'), rounded result"),
       col = 1:3, lwd = 3, bty = "n", cex = 0.9)
# No, we don't, which is not overly surprising. Therefore it is highly recommended
# NOT to use method="expected".
# => Replicate a game 20 times and use method="sample".

}
\seealso{
\code{\link{print.game}}, \code{\link{gameRep}} for a game repeated multiple times,
  \code{\link{league}}, \code{\link{tournament}}
}
