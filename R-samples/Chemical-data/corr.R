corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  result <- numeric(0)
  dir = paste(directory, "/", sep="")
  files <- list.files(dir)
  for (f in files) {
    data <- read.csv(paste(directory, "/", f, sep="")) 
    r <- data[complete.cases(data),]
    if (sum(complete.cases(data)) > threshold) {
      result <- c(result, cor(as.numeric(r$sulfate), as.numeric(r$nitrate), use="pairwise.complete.obs"))
    #sulfate <- r[,2]
    #//print(sulfate)
    #nitrate <- r[,3]
    #//print(nitrate)
    #cor(as.numeric(sulfate), as.numeric(nitrate), use="pairwise.complete.obs")
      

    }
  }
  result
}
