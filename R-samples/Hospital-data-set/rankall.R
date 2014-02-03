rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  outcomeData[, 11] <- as.numeric(outcomeData[, 11])
  outcomeData[, 17] <- as.numeric(outcomeData[, 17])
  outcomeData[, 23] <- as.numeric(outcomeData[, 23])
  
 
  
  if (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") {
    states <- names(table(outcomeData$State))
    colnames(outcomeData)[2] <- "hospital"
    colnames(outcomeData)[7] <- "state"
    res <- data.frame(matrix(ncol = 2))
    
    colnames(res)[1] <- "hospital"
    colnames(res)[2] <- "state"
    for (ss in states) {
      outcome2 <- subset(outcomeData, state %in% ss)
    
      if (outcome == "heart attack") {
        outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, outcome2$hospital),]
        outcome2 <- outcome2[!is.na(outcome2[,11]),]
      } else if (outcome == "heart failure") {
        outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, outcome2$hospital),]
        outcome2 <- outcome2[!is.na(outcome2[,17]),]
      } else {
        outcome2 <- outcome2[order(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, outcome2$hospital),]
        #print(outcome2[,23])
        outcome2 <- outcome2[!is.na(outcome2[,23]),]
        #print(outcome2[,23])
      }
      
      
      
    #print(ss)
      
      if (num == "best") {
       # print(outcome2[1,c(2,7)])
        #print(res)
        if (is.na(outcome2[1,c(2,7)])) {
          d <- c(NA, ss)
          rbind(res, d) -> res
        } else {
          rbind(res, outcome2[1,c(2,7)]) -> res
        }
      } else if (num == "worst") {
        num1 <- nrow(outcome2)
        if (is.na(outcome2[num1,c(2,7)])) {
          d <- c(NA, ss)
          rbind(res, d) -> res
        } else {
          #num1 <- nrow(outcomeData)
          rbind(res, outcome2[num1,c(2,7)]) -> res
        }
      } else {
        #print(outcome2[num,c(2,7)])
        if (is.na(outcome2[num,c(2,7)])) {
          d <- c(NA, ss)
          rbind(res, d) -> res
          #print(ss)
        } else {
          rbind(res, outcome2[num,c(2,7)]) -> res
        }
        #print(res)
      }
    }
    #print(res)
    res <- res[-1,]
   # print(res)
    res
  } else {
    stop("invalid outcome")
  }
}
