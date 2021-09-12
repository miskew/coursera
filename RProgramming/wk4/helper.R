## Check if state and outcome are valid input; halt otherwise.
validate <- function(validStates, state, outcome) {
    if (length(which(validStates == state)) == 0) {
        stop("invalid state")
    }
    
    if (outcome != "heart failure" & outcome != "heart attack" & outcome != "pneumonia") {
        stop("invalid outcome")
    }
}

getIndOfOutcomeCol <- function(colNames, outcome) {
    if (outcome == "heart failure") {
        indOfOutcomeCol <- which(colNames == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")
    } else if (outcome == "heart attack") {
        indOfOutcomeCol <- which(colNames == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")
    } else if (outcome == "pneumonia") {
        indOfOutcomeCol <- which(colNames == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    } else {
        stop("invalid outcome")
    }
    return(indOfOutcomeCol)
}

## Return hospital name in that state with lowest 30-day death rate
getRankedHospital <- function(f, indOfOutcomeCol, state, outcome, rank) {
    # Filter rows first by state.
    stateRows <- f[f$State == state,]
    if (is.numeric(rank) && rank > length(stateRows[,indOfOutcomeCol])) {
        return(NA)
    }

    # Next remove NAs and any values that can't be coerced to numeric, 
    # so we can coerce later when trying to do numeric comparisons (e.g. max/min).
    # Throw away any values which can't be used, based only on the outcome column.
    filteredIndexes = which(!is.na(stateRows[,indOfOutcomeCol]) & stateRows[,indOfOutcomeCol] != "Not Available" & canCoerce(stateRows[,indOfOutcomeCol], "numeric"))
    filtered <- stateRows[filteredIndexes,]
        
    if (rank == "best") {
        indsOfRank <- which(as.numeric(filtered[,indOfOutcomeCol]) ==  min(as.numeric(filtered[,indOfOutcomeCol]), na.rm = TRUE))    
    } else if (rank == "worst") {
        indsOfRank <- which(as.numeric(filtered[,indOfOutcomeCol]) ==  max(as.numeric(filtered[,indOfOutcomeCol]), na.rm = TRUE))    
    } else {
        # First reorder filtered rows by the outcome column in ascending order.
        filtered <- filtered[order(as.numeric(filtered[,indOfOutcomeCol]), filtered[,"Hospital.Name"]),]
        # Check to see if we have an ambiguous ranking from shared minimum values for the rank requested. If so, we want to
        # alphabetize those.
        minAtRank <- as.numeric(filtered[rank, indOfOutcomeCol])
        indsOfRank <- rank
    }
    
    ## if num is an integer, we need to use that to index into our sorted rate column? (ascending order) 

    if (length(indsOfRank) == 0) {
        return(NULL)
    }

    ## Return hospital name in that state with the given rank 30-day death rate.
    # If more than one minimum is found, tie break by selecting rank for item after ordering by hospital name.
    if (length(indsOfRank) > 1) {
        hospNames <- filtered[indsOfRank, "Hospital.Name"]
        return(hospNames[order(hospNames)[1]])
    }
    return(filtered[indsOfRank, "Hospital.Name"])
}