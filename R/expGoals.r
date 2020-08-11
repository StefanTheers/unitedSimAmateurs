#' Calculation of expected goals
#'
#' Calculates the expected goals of \code{x1} vs \code{x2} where
#'   \code{ x1 = [t, a, v, m, s] } and
#'   \code{ x2 = [t, a, v, m, s] }.
#'
#' @param x1 [\code{integer(5)}]\cr
#'   named team vector of team 1
#' @param x2 [\code{integer(5)}]\cr
#'   named team vector of team 2
#' @return \code{numeric(1)}, expected goals of team 1 vs. team 2
#' @export
#'
#' @examples
#' x1 <- c(t = 0,  a = 10, v = 16, m = 16, s = 48)
#' x2 <- c(t = 10, a = 10, v = 36, m = 12, s = 12)
#' expGoals(x1, x2)
#' expGoals(x2, x1)
#'
expGoals <- function(x1, x2) {
  chances <- (  max(0, round(x1["s"] - (x2["v"] + x2["a"])))
              + max(0, round(1/2 * (x1["m"] - x2["m"])))
              + max(0, round(1/4 * (x1["v"] - x2["s"]))))
  goals <- chances * (15 - x2["a"]) / 15 * (14 - x2["t"]) / 14
  attributes(goals) <- NULL
  return(goals)
}


