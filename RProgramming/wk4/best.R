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

    colToSearch <- rowsForState[,colInd]
    # Remove NAs and any values that can't be coerced to numeric
    filtered <- colToSearch[which(!is.na(colToSearch) & colToSearch != "Not Available" & canCoerce(colToSearch, "numeric"))]
    print(filtered)
    minVal <- min(as.numeric(filtered), na.rm = TRUE)
    print(minVal)
    indOfMins <- which(canCoerce(colToSearch, "numeric") & as.numeric(colToSearch) == minVal)
    print(indOfMins)
    if (length(indOfMins) == 0) {
        return(NULL)
    }

    # If more than one minimum is found, tie break by ordering by hospital name and returning first item.
    if (length(indOfMins) > 1) {
        hospNames <- rowsForState[indOfMins, "Hospital.Name"]
        print(hospNames)
        return(hospNames[order(hospNames)[1]])
    }
    return(rowsForState[indOfMins, "Hospital.Name"])
}