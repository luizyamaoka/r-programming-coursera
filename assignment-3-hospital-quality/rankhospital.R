rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  if (!state %in% c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")) {
    stop("invalid state")
  }
  
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if (num == "best") {
    position <- 1
  } else if (num == "worst") {
    position <- nrow(state_data)
  } else {
    position <- as.numeric(num)
    if (position > nrow(state_data)) {
      return(NA)
    }
  }
  state_data[order(state_data[column], state_data["Hospital.Name"], decreasing=FALSE),][position,1]
}