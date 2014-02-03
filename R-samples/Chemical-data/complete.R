complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  #file = paste(getwd(), "/", directory, "/", sep="")
  existingDF <- data.frame()
  ids = id
  for(id in ids) {
    if (id < 10) {
      v1 <- paste("00", id, sep="")
    } else if (id < 100) {
      v1 <- paste("0", id, sep="")
    } else {
      v1 <- id
    }
    csvfile = paste(getwd(), "/", directory, "/", v1, ".csv", sep="")
    data <- read.csv(csvfile)
    #r <- complete.cases(data)
    r <- data[complete.cases(data), ]
    nobs <- nrow(r)
    t <- data.frame(id, nobs)
    existingDF <- rbind(existingDF,t)
    
  }
  print(existingDF)
}