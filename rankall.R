#setwd("C:/RProjects/ProgrammingAssignment3")
outcome <- "heart failure"
num <- 10

rankall <- function(outcome, num = "best") {
  setwd("C:/RProjects/ProgrammingAssignment3")
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  outcometab <- data.frame(c(11, 17, 23), row.names = c("heart attack", "heart failure", "pneumonia"))
  if (!(outcome %in% row.names(outcometab))) stop("invalid outcome") 
  data <- data[c(2,7,as.numeric(outcometab[outcome,]))]
  sortdata <- data[order(as.numeric(data[, 3]), as.character(data[, 1])), ]
  spsortdata <- split(sortdata, sortdata$State)

  
  rankHospitals <- function(x, num) {
      if (num=="best") {
        x[1, 1]
      } else if (num=="worst") {
        x[nrow(x), 1]
      } else {
        x[num, 1]
      }
  }
  	
  result <- lapply(spsortdata, rankHospitals, num)
  data.frame(hospital = unlist(result), state = names(result), row.names = names(result))
}