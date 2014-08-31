
################################################################################
##  Function: rankall
################################################################################


rankall <- function(outcome, num= "best") {
  ##outcome <- "heart attack"
  ##num <- 5
  
  if (num == "best") {num <- 1}
  
  
  ## Check that outcome is valid
  
  if (outcome != 'heart attack' && outcome != 'heart failure' && outcome != 'pneumonia') {
    return(stop("invalid outcome"))
  }
  
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeData <- outcomeData[,c(2,7,11,17,23)]
  names(outcomeData) <- c("hospital","state","HeartAttack","HeartFailure","Pneumonia")
  outcomeData$HeartAttack <- as.numeric(outcomeData$HeartAttack)
  outcomeData$HeartFailure <- as.numeric(outcomeData$HeartFailure)
  outcomeData$Pneumonia <- as.numeric(outcomeData$Pneumonia)
  
  #Get a unique list of states.
  stateList <- unique(outcomeData$state)
  
  #initialize data frame
  ranked <- data.frame(hospital=character(), state=character())
  tempranked <- data.frame(hospital="", state="")
  #   testrank <- data.frame(hospital="test3", state="test3state")
  #   newtest <- rbind(newtest,testrank)
  for (abbr in stateList) {
    
    ## Trim and sort
    outcomeTrimmed <- subset(outcomeData, state == abbr)
    
    if (outcome == 'heart attack') {
      outcomeTrimmed <- outcomeTrimmed[,c(1,2,3)]
      outcomeTrimmed <- na.omit(outcomeTrimmed)
      outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$HeartAttack,outcomeTrimmed$hospital),]
      
      if (num == "worst") {
        lastrow <- nrow(outcomeTrimmed)
        tempranked$hospital <- outcomeTrimmed[lastrow,1]
        tempranked$state <- abbr
        ranked <- rbind(ranked,tempranked)
      } else {tempranked$hospital <- outcomeTrimmed[num,1]
              tempranked$state <- abbr
              ranked <- rbind(ranked,tempranked)}
      
    }
    
    
    if (outcome == 'heart failure') {
      outcomeTrimmed <- outcomeTrimmed[,c(1,2,4)]
      outcomeTrimmed <- na.omit(outcomeTrimmed)
      outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$HeartFailure,outcomeTrimmed$hospital),]
      
      if (num == "worst") {
        lastrow <- nrow(outcomeTrimmed)
        tempranked$hospital <- outcomeTrimmed[lastrow,1]
        tempranked$state <- abbr
        ranked <- rbind(ranked,tempranked)
      } else {tempranked$hospital <- outcomeTrimmed[num,1]
              tempranked$state <- abbr
              ranked <- rbind(ranked,tempranked)}
    }
    
    
    if (outcome == 'pneumonia') {
      outcomeTrimmed <- outcomeTrimmed[,c(1,2,5)]
      outcomeTrimmed <- na.omit(outcomeTrimmed)
      outcomeTrimmed <- outcomeTrimmed[order(outcomeTrimmed$Pneumonia,outcomeTrimmed$hospital),]
      
      if (num == "worst") {
        lastrow <- nrow(outcomeTrimmed)
        tempranked$hospital <- outcomeTrimmed[lastrow,1]
        tempranked$state <- abbr
        ranked <- rbind(ranked,tempranked)
      } else {tempranked$hospital <- outcomeTrimmed[num,1]
              tempranked$state <- abbr
              ranked <- rbind(ranked,tempranked)}
      
    }  
    
  } # end loop
  
  ranked <- ranked[order(ranked$state),]
  return(ranked)
  
} # End of function.

## Test Values

##head(rankall("heart attack", 20), 10)
