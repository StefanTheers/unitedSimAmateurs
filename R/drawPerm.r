#' Calculation of permutations of all possible draws
#' @param n integer(1), number of teams
#' @param sample logical(1), TRUE if only a sample of size sample.size
#'   of all possible draws is returned (with replacement of draws)
#' @param sample.size integer(1), sample size
#' @return (n x m)-dim. matrix where m=factorial(n) if sample=FALSE or
#'   m=sample.size if sample=TRUE.
#' @note Stay away from simulating all possible draws if there are more
#'   than 8 teams competing in your tournament. Use sample=TRUE.
#' @export
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
