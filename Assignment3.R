#  R Programming
#  Assignment 3
#  Jason Green
#  8/28/2014

# http://hospitalcompare.hhs.gov
# outcome-of-care-measures.csv
#     Info about 30-day mortality and readmission rates for 
#     heart attacks, heart failure, and pneumonia for over 4,000 hospitals.
#
# hospital-data.csv
#     Info about each hospital.
#
# Hospital_Revised_Flatfiles.pdf
#



# Set Working Directory
setwd("~/GitHub/RProgramming3/")


#  This assignment will be graded using unit tests executed via the submit script that you run on your computer. To obtain the submit script, run the following code in R:
   source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
   
#   Or you can download the script to your working directory (NOTE: You may need to rename the script to be "submitscript3.R"). Then source it locally via
#   source("submitscript3.R")
   
#   The first time you run the submit script it will prompt you for your Submission login and Submission password. 
#   These can be found at the top of the Programming Assignments page. To execute the submit script, type
   submit()


################################################################################
##  Load Data / Initial Analysis
################################################################################
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

# Simple Histogram
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])



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

## Test Values
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")


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
rankhospital("TX", "heart failure",4)
rankhospital("TX", "heart failure")
rankhospital("MD", "heart attack", "worst")
rankhospital("MD", "pneumonia",5000)
rankhospital("BB", "heart attack")
rankhospital("NY", "hert attack")
  
  


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

head(rankall("heart attack", 20), 10)
