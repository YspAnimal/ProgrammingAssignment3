#setwd("C:/RProjects/ProgrammingAssignment3")
#outcome <- "heart failure"
#state <- "TX"

rankhospital <- function(state, outcome, num = "best") {
  setwd("C:/RProjects/ProgrammingAssignment3")
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  stateV <- unique(data$State)
  outcometab <- data.frame(c(11, 17, 23), row.names = c("heart attack", "heart failure", "pneumonia"))
  if (!(state %in% stateV)) stop("invalid state") 
  if (!(outcome %in% row.names(outcometab))) stop("invalid outcome") 
  data <- data[which(data$State == state), ]
  data <- data[complete.cases(data), ]
  data <- data[c(2,7,as.numeric(outcometab[outcome,]))]
  sortdata <- data[order(as.numeric(data[, 3]), data[, 2]), ]
  rownames(sortdata) <- 1:nrow(sortdata)
  if (is.numeric(num)) {
      return(sortdata[num, 1])
  } else if (num == "worst") {
      return(sortdata[nrow(sortdata), 1])
  } else if (num == "best"){
      return(sortdata[1, 1])
  } else {
    return(NA)
  }
}