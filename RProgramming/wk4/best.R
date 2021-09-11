# Return hospital name with the lowest mortality rate for a given outcome in a given state.
best <- function(state, outcome) {
    ## Read outcome data
    f <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    statesInFile <- unique(f$State)
    if (length(which(statesInFile == state)) == 0) {
        stop("invalid state")
    }

    if (outcome == "heart failure") {
        colInd <- 17
    } else if (outcome == "heart attack") {
        colInd <- 11
    } else if (outcome == "pneumonia") {
        colInd <- 23
    } else {
        stop("invalid outcome")
    }

    ## Return hospital name in that state with lowest 30-day death rate
    rowsForState = f[f$State == state,]

    # Remove NAs and any values that can't be coerced to numeric so we can coerce later.
    colToSearch <- rowsForState[,colInd]
    filtered <- rowsForState[which(!is.na(colToSearch) & colToSearch != "Not Available" & canCoerce(colToSearch, "numeric")),]    
    
    indOfMins <- which(as.numeric(filtered[,colInd]) ==  min(as.numeric(filtered[,colInd]), na.rm = TRUE))
    if (length(indOfMins) == 0) {
        return(NULL)
    }

    # If more than one minimum is found, tie break by selecting first item after ordering by hospital name.
    if (length(indOfMins) > 1) {
        hospNames <- filtered[indOfMins, "Hospital.Name"]
        return(hospNames[order(hospNames)[1]])
    }
    return(filtered[indOfMins, "Hospital.Name"])
}