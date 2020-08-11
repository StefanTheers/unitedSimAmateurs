#' Read data from ZAT \code{.ods} files
#'
#' This function automatically reads data from ZAT \code{.ods} files that have to be
#' formatted as defined in \href{https://gitlab.com/fchalligalli/bfbunited}{\code{bfbunited}}.
#'
#' @param path [\code{character(1)}]\cr
#'   path of folder with \code{.ods} lineup files
#' @param nmr [\code{character(1)}]\cr
#'   name of NMR \code{.ods} file in the same folder
#' @return correctly named lineup matrix that can be used as input for \code{\link{tournament}} or \code{\link{league}}
#' @export
#'
readLineups <- function(path = "data", nmr = "NMR-Amateure.ods") {
  teamsFiles <- list.files(path, pattern = ".ods")
  teamsFiles <- teamsFiles[- grep(nmr, teamsFiles)]
  teamsVec <- c()
  nmrDat <- as.matrix(readODS::read_ods(file.path(path, nmr), col_types = NA, col_names = FALSE))
  for(f in teamsFiles) {
    dat <- as.matrix(readODS::read_ods(file.path(path, f), col_types = NA))
    pos <- grep("Amateure:", dat)
    pos <- which(dat == dat[pos], arr.ind = TRUE)
    if(nrow(pos) != 1 | any(colnames(pos) != c("row", "col")) | !is.integer(pos[1, ]))
      stop("Cannot find 'Amateure:' entry in:\n", f)
    lineup <- dat[pos[1, 1] + 1, pos[1, 2]]
    team <- strsplit(f, split = ".ods")[[1]]
    if(!isTRUE(1 == grep("1?[0-9]-1?[0-9]-[0-9]{1,2}-[0-9]{1,2}-[0-9]{1,2}", lineup))) {
      cat("Amateurs' lineup does not comply with regular expression for:\n", f, "\nlineup:", lineup)
      lineup <- nmrDat[nmrDat[, 1] == team, 2]
      cat("\nUse NMR lineup instead:", lineup, "\n\n")
    }
    teamsVec <- c(teamsVec, setNames(lineup, team))
  }

  # Check for NMR teams, i.e. teams for which no .ods file exist:
  nmrTeams <- nmrDat[!(nmrDat[, 1] %in% names(teamsVec)), ]
  teamsVec <- c(teamsVec, setNames(nmrTeams[, 2], nmrTeams[, 1]))

  retVal <- sapply(teamsVec, function(x) strsplit(x, split = "-"))
  retVal <- t(apply(do.call(rbind, retVal), 1, as.integer))
  colnames(retVal) <- c("t", "a", "v", "m", "s")
  return(retVal)
}
