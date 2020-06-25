

rankall <- function(outcome, num = "best") { ## Read outcome data
  ## get directory from environment;
  ## if it's not there, use default
  directory<-Sys.getenv("directory")
  if (is.null(directory)){
    directory="C:\\Users\\grier006\\Documents\\R\\R-week4"
  }
  setwd(directory)
  ## Read outcome data
  data<- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that outcome is valid
  
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
  
  # remove missing data
  goodCases<-complete.cases(myData)
  myData=myData[goodCases,]
  # Make the output more readable
  names(myData)[3]<-"Rate"
  # Rate should be numeric for sort
  myData[,3]<-suppressWarnings(as.numeric(myData[,3]))  
  
  # Split by state
  StateData<-split(myData, myData$State)
  
  ## For each state, find the hospital of the given rank
  ans <- lapply(StateData, function (s, num){
    s<-s[order(s$Rate, s$Hospital.Name),]
    if (num == "best"){
      return(s[1])
    }
    else if (num == "worst"){
     # return(s[nrow(s)])
      return (s[which.max(s$Rate),])
    }
    else if (is.numeric(num)){
      if (num > nrow(s)){
        result<-"NA"
      }
      else{
        result<-s[num,]
      }
    }
    
  }, num)
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  return(ans)
}
result<-rankall("heart attack", 20)
head(result)
result<-rankall("pneumonia", "worst")
tail(result)
