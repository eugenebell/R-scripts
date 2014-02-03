

count <- function(cause = NULL) {
  if (is.null(cause)) {
    stop("Cause not found")
  }
  if (cause != "asphyxiation" && cause != "blunt force" && cause != "other" && cause != "shooting"
      && cause != "stabbing" && cause != "unknown") {
    stop("Invalid cause")
  }
  setwd("/Users/eugenebell/R/Lecture4/")
  h <- readLines("homicides.txt")
  len <- 0
  if (cause == "asphyxiation") {
    len <- length(grep("Cause: [Aa]sphyxiation", h))
  } else if (cause == "blunt force") {
    len <- length(grep("Cause: [bB]lunt [fF]orce", h))
  }  else if (cause == "other"){
    len <- length(grep("Cause: [Oo]ther", h))
  }  else if (cause == "shooting") {
    len <- length(grep("Cause: [Ss]hooting", h))
  }  else if (cause == "stabbing") {
    len <- length(grep("Cause: [Ss]tabbing", h))
  }  else if (cause == "unknown") {
    len <- length(grep("Cause: [Uu]nknown", h))
  }
  return(len)
  ## Check that "cause" is non-NULL; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
  
}
