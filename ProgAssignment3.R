# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# 
# outcome[, 11] <- as.numeric(outcome[, 11])
# ## You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])
setwd("C:/RProjects/ProgrammingAssignment3")
outcome <- "heart attack"
state <- "TX"

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  stateV <- unique(data$State)
  outcomeV <- c("heart attack", "heart failure", "pneumonia")
  if (!(state %in% stateV)) stop("invalid state") 
  if (!(outcome %in% outcomeV)) stop("invalid outcome") 
  data <- data[which(data$State == state), ]
  data <- data[complete.cases(data), ]

  
  
    
}