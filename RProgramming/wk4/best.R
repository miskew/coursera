source("helper.R")

# Return hospital name with the lowest mortality rate for a given outcome in a given state.
best <- function(state, outcome) {
    ## Read outcome data
    f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check if state and outcome are valid input.
    ## Get column index of requested outcome or halt program.
    statesInFile <- unique(f$State)
    validate(statesInFile, state, outcome)
    colNames <- names(f)
    indOfOutcomeCol <- getIndOfOutcomeCol(colNames, outcome)

    getRankedHospital(f, indOfOutcomeCol, state, outcome, "best")
}