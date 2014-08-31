#  R Programming
#  Assignment 3
#  Jason Green
#  8/28/2014

################################################################################
##  Function: best
################################################################################
best <- function(state, outcome) {
  #fortesting  state <- "AL"
  #fortesting  outcome <- "heart attack"
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeTrimmed <- outcomeData[,c(2,7,11,17,23)]
  names(outcomeTrimmed) <- c("HospitalName","State","HeartAttack","HeartFailure","Pneumonia")
  outcomeTrimmed$HeartAttack <- as.numeric(outcomeTrimmed$HeartAttack)
  outcomeTrimmed$HeartFailure <- as.numeric(outcomeTrimmed$HeartFailure)
  outcomeTrimmed$Pneumonia <- as.numeric(outcomeTrimmed$Pneumonia)
  
  ## Check that state and outcome are valid
  if (length(outcomeTrimmed$State[outcomeTrimmed$State==state]) == 0) {
    return(stop("invalid state"))  }
  
  if (outcome != 'heart attack' && outcome != 'heart failure' && outcome != 'pneumonia') {
    return(stop("invalid outcome"))
  }
  
  ## Return hospital name in that state with the lowest 30-day death rate
  ## Trim and sort
  outcomeTrimmed <- subset(outcomeTrimmed, State == state)
  
  if (outcome == 'heart attack') {
    outcomeTrimmed <- outcomeTrimmed[,c(1,2,3)]
    outcomeTrimmed <- na.omit(outcomeTrimmed)
    outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$HeartAttack,outcomeTrimmed$HospitalName),]
    return(outcomeTrimmed[1,1])
  }
  
  
  if (outcome == 'heart failure') {
    outcomeTrimmed <- outcomeTrimmed[,c(1,2,4)]
    outcomeTrimmed <- na.omit(outcomeTrimmed)
    outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$HeartFailure,outcomeTrimmed$HospitalName),]
    return(outcomeTrimmed[1,1])
  }
  
  
  if (outcome == 'pneumonia') {
    outcomeTrimmed <- outcomeTrimmed[,c(1,2,5)]
    outcomeTrimmed <- na.omit(outcomeTrimmed)
    outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$Pneumonia,outcomeTrimmed$HospitalName),]
    return(outcomeTrimmed[1,1])
  }
} # End of function.