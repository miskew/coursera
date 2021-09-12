source("rankhospital.R")
source("helper.R")

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check if state and outcome are valid input; if so, get column index of requested outcome.    
    if (outcome != "heart failure" & outcome != "heart attack" & outcome != "pneumonia") {
        stop("invalid outcome")
    }
    colNames <- names(f)
    indOfOutcomeCol <- getIndOfOutcomeCol(colNames, outcome)
    statesInFile <- unique(f$State)
    statesInFile <- statesInFile[order(statesInFile)]
    ## For each state, find the hospital of the given rank
    result <- lapply(statesInFile, rankhospital, outcome, num)
    hospitalNames <- sapply(result, '[[', 1)

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data.frame(statesInFile, hospitalNames)
}