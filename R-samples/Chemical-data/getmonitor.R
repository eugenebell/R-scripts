getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  temp = list.files(path=directory, pattern="*.csv")
  if (id < 10) {
    id = paste("00", id, sep="")
  } else if (id < 100) {
    id = paste("0", id, sep="")
  }
  csvfile = paste(getwd(), "/", directory, "/", id, ".csv", sep="")
  data <- read.csv(csvfile)
  #data <- na.omit(c)
  if (summarize) {
   summary(data)
  }
  #else {
    return (data)
  #}
}