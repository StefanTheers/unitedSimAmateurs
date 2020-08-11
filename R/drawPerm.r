#' Calculation of permutations of possible draws
#'
#' For \code{n} teams \code{1,...,n}, the function either calculates all possible \code{n!} draws
#' or samples \code{sample.size} random draws (with repetition).
#'
#' @param n [\code{integer(1)}]\cr
#'   number of teams
#' @param sample [\code{logical(1)}]\cr
#'   TRUE if only a sample of size \code{sample.size}
#'   of all possible draws is returned (with replacement of draws)
#' @param sample.size [\code{integer(1)}]\cr
#'   sample size
#' @return (\code{n} x \code{m})-dim. matrix where \code{m=factorial(n)} if \code{sample=FALSE} or
#'   \code{m=sample.size} if \code{sample=TRUE}.
#' @note Stay away from simulating all possible draws if there are more
#'   than 8 teams competing in your tournament. Use \code{sample=TRUE}.
#' @seealso \code{\link{simTournamentPerm}}
#' @export
#'
#' @examples
#' drawPerm(4, sample = FALSE)
#' drawPerm(4, sample = TRUE, sample.size = 15)
#'
drawPerm <- function(n, sample = TRUE, sample.size = 10) {
  if(!sample) {
    if(n > 8)
      warning("Note that n > 8. It might be computationally infeasible to ",
              "continue as there are ", factorial(n), " possible draws.")
    retVal <- t(gtools::permutations(n = n, r = n))    # the function is slow :( (recursive)
  } else {
    retVal <- replicate(sample.size, sample(1:n, n, replace = FALSE))
  }
  return(retVal)
}
