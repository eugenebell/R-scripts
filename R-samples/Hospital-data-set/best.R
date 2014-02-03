#Part 5

best <- function(state, outcome) {
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #mx11 <- is.na(outcome[, 11])
  #a11 <- median(outcome[, 11][!mx11])
  outcomeData[, 11] <- as.numeric(outcomeData[, 11])
  outcomeData[, 17] <- as.numeric(outcomeData[, 17])
  outcomeData[, 23] <- as.numeric(outcomeData[, 23])
  states <- names(table(outcomeData$State))
  ## Check that state and outcome are valid
  states <- names(table(outcomeData$State))
  if (!state %in% states) {
    stop("invalid state")
  }
  if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") {
    outcome2 <- subset(outcomeData, State %in% state)
    #summary(outcome2)
    #outcome2[,2]
    
    if (outcome == "heart attack") {
      outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,outcome2$Hospital.Name),]
    } else if (outcome == "heart failure") {
      outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcome2$Hospital.Name),]
    } else {
      outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcome2$Hospital.Name),]
    }
    #outcome2[1,2]
    #outcome2[,23]
    return(outcome2[1,2])
    #newdata <- outcome2[order(outcome2$Hospital.Name),] 
    #newdata[,2]
    #summary(newdata)
   # table(newdata$Hospital.Name)
  } else {
    stop("invalid outcome", call. = FALSE)
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
