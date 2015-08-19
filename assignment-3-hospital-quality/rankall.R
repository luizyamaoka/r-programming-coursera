rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  } else if (outcome == "heart attack") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } 
  
  data[,column] <- as.numeric(data[,column])
  # filter data frame from state
  state_data <- data[which(data$State == state), c("Hospital.Name", column)]
  state_data <- state_data[complete.cases(state_data),]
  
  ## For each state, find the hospital of the given rank
  states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "VI", "WA", "WV", "WI", "WY")
  
  return_data_frame <- data.frame(hospital=character(), state=character())
  for (state in states) {
    
    state_data <- data[which(data$State == state), c("Hospital.Name", column)]
    state_data <- state_data[complete.cases(state_data),]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == "best") {
      hospital <- state_data[order(state_data[column], state_data["Hospital.Name"], decreasing=FALSE),][1,1]
    } else if (num == "worst") {
      hospital <- state_data[order(state_data[column], state_data["Hospital.Name"], decreasing=FALSE),][nrow(state_data),1]
    } else {
      position <- as.numeric(num)
      if (position > nrow(state_data)) {
        hospital <- NA
      } else {
        hospital <- state_data[order(state_data[column], state_data["Hospital.Name"], decreasing=FALSE),][position,1]
      }
    }
    
    return_data_frame <- rbind(return_data_frame, data.frame(hospital=hospital, state=state))
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  return_data_frame
}