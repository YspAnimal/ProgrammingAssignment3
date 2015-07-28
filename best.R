# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
# head(outcome)
# 
# outcome[, 11] <- as.numeric(outcome[, 11])
# ## You may get a warning about NAs being introduced; that is okay
# hist(outcome[, 11])
#setwd("C:/RProjects/ProgrammingAssignment3")
#outcome <- "pneumonia"
#state <- "NY"

best <- function(state, outcome) {
  setwd("C:/RProjects/ProgrammingAssignment3")
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  stateV <- unique(data$State)
  outcometab <- data.frame(c(11, 17, 23), row.names = c("heart attack", "heart failure", "pneumonia"))
  ##outcomeN <- c(11, 17, 23)
  ##outcomtab <- 
  if (!(state %in% stateV)) stop("invalid state") 
  if (!(outcome %in% row.names(outcometab))) stop("invalid outcome") 
  data <- data[which(data$State == state), ]
  data <- data[complete.cases(data), ]
  orderdata <- data[order(as.numeric(data[, outcometab[outcome,]]), data[, 2]), ]
  return(orderdata[1,2])
}