agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is
  ## given
  ## Return integer containing count of homicides for that age
  if (is.null(age)) {
    stop("age not found")
  }
  setwd("/Users/eugenebell/R/Lecture4/")
  h <- readLines("homicides.txt")
  #a <- sub("male, | years old</dd>.*", "", h)
  #print(a)
  b <- gsub(".*Age: | year.* old</dd>.*", "", h)
  b <- gsub(".*male, ", "", b)
  b <- gsub("[a-z,A-Z]", "", b)
  #print(b)
  e <- paste("^",age,"$",sep="")
  #e <- gsub("^\\s+|\\s+$", "", e)
  print(e)
  c <- length(grep(e, b))
  #b <- gsub(".*Age: | year old</dd>.*", "", h)
  #b <- gsub(".*Age: | year old</dd>.*", "", h)
  #m <- regmatches(h, res)
  print(c)
  return(c)
}