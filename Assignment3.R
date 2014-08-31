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
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  
  
}


################################################################################
##  Function: rankall
################################################################################


