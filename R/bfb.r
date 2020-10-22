#' Official BFB amateur tournament results of Season 45
#'
#' A \code{\link[data.table]{data.table}} with the official BFB results of the three
#' amateur tournaments played in Season 45 with p = 81, 71 and 75 WP (value points)
#' each involving 64 teams.
#' @docType data
#' @usage data(bfb)
#' @format
#' \code{Verein} contains the team names,
#' \code{Platz} the official position.
#' The \code{pr1} to \code{pr3} columns are equivalent to the
#' tournament result columns \code{Turnier1} to \code{Turnier3}.
#' \code{Punkte} and \code{prSum} are equal and contain the number of points
#' won by each team (1 point = 1 game won). Three tournaments have been played with 64 teams,
#' i.e. a maximum of 3 * 6 = 18 points can be won by a single team if it wins all three of them.
#' This has not happened though.
#'
#' @keywords datasets
#' @examples
#' data(bfb)
#' bfb
#'
#' # plot the tournament results:
#' X <- as.matrix(bfb[, pr1:pr3])
#' rownames(X) <- bfb[, Verein]
#' colnames(X) <- paste("Turnier", 1:3)
#' library("plot.matrix")
#' op <- par(mar = c(5,12,4,2) + 0.1)
#' plot(X, las = 1, cex.axis = 0.85, fmt.cell='%.0f', cex = 0.85,  key = NULL,
#'   ylab = "", xlab = "", main = "Official BFB amateur tournament results of Season 45")
#' par(op)
#'
"bfb"
