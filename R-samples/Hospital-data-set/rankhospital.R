rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
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
      outcome2 <- outcome2[!is.na(outcome2[,11]),]
    } else if (outcome == "heart failure") {
      outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,outcome2$Hospital.Name),]
      outcome2 <- outcome2[!is.na(outcome2[,17]),]
    } else {
      outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,outcome2$Hospital.Name),]
      outcome2 <- outcome2[!is.na(outcome2[,23]),]
    }
    #outcome2[1,2]
    #outcome2[,23]
    if (num == "best") {
      outcome2[1,2]
    } else if (num == "worst") {
      num1 <- nrow(outcome2)
      outcome2[num1,2]
    } else {
      outcome2[num,2]
    }
  } else {
    stop("invalid outcome")
  }
}




