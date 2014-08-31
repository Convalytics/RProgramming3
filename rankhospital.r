
################################################################################
##  Function: rankhospital
################################################################################
rankhospital <- function(state, outcome, num= "best") {
  ##state <- "MD"
  ##outcome <- "heart attack"
  ##num <- "worst"
  
  if (num == "best") {num <- 1}
  
  ## Read outcome data
  outcomeTrimmed <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeTrimmed <- outcomeTrimmed[,c(2,7,11,17,23)]
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
    
    if (num == "worst") {
      lastrow <- nrow(outcomeTrimmed)
      return(outcomeTrimmed[lastrow,1])
    } else {return(outcomeTrimmed[num,1])}
    
  }
  
  
  if (outcome == 'heart failure') {
    outcomeTrimmed <- outcomeTrimmed[,c(1,2,4)]
    outcomeTrimmed <- na.omit(outcomeTrimmed)
    outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$HeartFailure,outcomeTrimmed$HospitalName),]
    
    if (num == "worst") {
      lastrow <- nrow(outcomeTrimmed)
      return(outcomeTrimmed[lastrow,1])
    } else {return(outcomeTrimmed[num,1])}
  }
  
  
  if (outcome == 'pneumonia') {
    outcomeTrimmed <- outcomeTrimmed[,c(1,2,5)]
    outcomeTrimmed <- na.omit(outcomeTrimmed)
    outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$Pneumonia,outcomeTrimmed$HospitalName),]
    
    if (num == "worst") {
      lastrow <- nrow(outcomeTrimmed)
      return(outcomeTrimmed[lastrow,1])
    } else {return(outcomeTrimmed[num,1])}
    
  }
} # End of function.

## Test Values
# rankhospital("TX", "heart failure",4)
# rankhospital("TX", "heart failure")
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MD", "pneumonia",5000)
# rankhospital("BB", "heart attack")
# rankhospital("NY", "hert attack")
# 
