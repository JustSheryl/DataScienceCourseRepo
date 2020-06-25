# Given a state, and outcome and a rank, find the
# hospital in that state with the ranking for
# that outcome 

rankhospital <- function(state, outcome, num = "best") { 
  ## get directory from environment;
  ## if it's not there, use default
  directory<-Sys.getenv("directory")
  if (is.null(directory)){
    directory="C:\\Users\\grier006\\Documents\\R\\R-week4"
  }
  setwd(directory)
  ## Read outcome data
  data<- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  
  if(!(state %in% data$State)){
    stop("Invalid State")
  }
  
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("Invalid Outcome")
  }
  
  ## per Hospital_Revised_Flatfiles.pdf
  ## Heart Attack mortality rate:  col. 11
  ## Heart Failure mortaility rate:  col. 17
  ## Pneumonia mortality rate:  col. 23
  outcomeCol = NULL
  if (outcome == "heart attack"){
    outcomeCol<-11
  }
  else if (outcome == "heart failure"){
    outcomeCol<-17
  }
  else{
    outcomeCol<-23
  }
 
  # We only need 3 of the 40+ columns:
  # State (7), Hospital (2), outcomeCol
  myData<-data[,c(2, 7, outcomeCol)]
  
  library(dplyr)
  
  ## filter the state we want
  StateData<-filter(myData, myData$State == state)

  # remove missing data
  goodCases<-complete.cases(StateData)
  StateData=StateData[goodCases,]
  # Make the output more readable
  names(StateData)[3]<-"Rate"
  # Rate should be numeric for sort
  StateData[,3]<-suppressWarnings(as.numeric(StateData[,3]))
  # Sort by rate then by name
  StateData<-arrange(StateData,StateData$Rate,StateData$Hospital.Name)
  
  #just for grins, get the best and the worst
  best<-StateData[which.min(StateData$Rate),]
 # print(best)
  worst<-StateData[which.max(StateData$Rate),]
#  print(worst)
  # NOW - get the hospital by rank requested
  ## Return hospital name in that state with the given rank ## 30-day death rate
  if(num == "best"){
    result<-best
  }
  if (num == "worst"){
    result<-StateData[which.max(StateData$Rate),]
  }
  if (is.numeric(num)){
    if (num > which.max(StateData$Rate)){
      result<-"NA"
    }
    else{
      result<-StateData[num,]
    }
  }
  result
}
result<-rankhospital("TX", "heart failure", "best")
result
result<-rankhospital("MD", "heart attack", "worst")
result
