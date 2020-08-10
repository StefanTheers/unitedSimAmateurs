#' Read data from ZAT .ods files
#' @param path character(1), path of folder with lineup files
#' @param nmr character(1), name of NMR file in the same folder
#' @return correctly named lineup matrix that can be used as input for tournament()
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
