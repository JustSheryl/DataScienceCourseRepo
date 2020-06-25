#best() reads the outcome-of-care-measures.csv ???le 
#and returns a character vector with the name of the 
#hospital that has the best (i.e. lowest) 30-day 
#mortality for the speci???ed outcome in that state. 

best <- function(state, outcome) { 

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
  
  ## Return hospital name in that state with lowest 30-day death 
  ## rate
   
   # We only need 3 of the 40+ columns:
   # State (7), Hospital (2), outcomeCol
   myData<-data[,c(2, 7, outcomeCol)]
   
   library(dplyr)
   
   ## filter the state we want
   StateData<-filter(myData, myData$State == state)

   ## Sort by Hospital Name
   #StateData<-arrange(StateData, StateData$Hospital.Name)

   # remove missing data
   goodCases<-complete.cases(StateData)
   StateData=StateData[goodCases,]
   # Make the output more readable
   names(StateData)[3]<-"Rate"
   # Rate should be numeric for sort
   StateData[,3]<-suppressWarnings(as.numeric(StateData[,3]))
   # find the best outcome
   StateData<-arrange(StateData,StateData$Rate,StateData$Hospital.Name)
   BestHospital<-head(StateData,1)
   #debug
   #print(StateData)
}
#best("TX","heart failure")

