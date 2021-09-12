source("helper.R")
source("best.R")

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check if state and outcome are valid input; if so, get column index of requested outcome.    
    statesInFile <- unique(f$State)
    validate(statesInFile, state, outcome)
    colNames <- names(f)
    indOfOutcomeCol <- getIndOfOutcomeCol(colNames, outcome)

    ## check if num is best. if so, use function we already have to answer question.
    ## otherwise proceed
    if (!(is.numeric(num)) && !(num == "best" || num == "worst")) {
       stop("invalid rank")
    }

    getRankedHospital(f, indOfOutcomeCol, state, outcome, num)
}