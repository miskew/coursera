corr <- function (directory, threshold = 0) {
    files <- complete(directory)
    rowsAboveThreshold <- files[which(files$nobs > threshold),]

    # Read in data for all monitors above threshold
    file_list <- list.files(directory, full.names = TRUE)
    if(length(file_list) == 0) { 
        return(numeric()) 
    }

    # Create df for all data with ids which are above threshold
    allData <- data.frame()
    for (i in 1:dim(rowsAboveThreshold)[1]) {        
        row <- rowsAboveThreshold[i,]
        filename <- file_list[row$id]
        if(!file.exists(filename)) {
            return(numeric()) 
        }
        allData <- rbind(allData, read.csv(file_list[row$id]))
    }

    # Using id from rows above threshold, get the columns for sulfate and nitrate
    # and calculate the correlation between the two vectors
    answer = c()
    for (i in 1:dim(rowsAboveThreshold)[1]) {
        row <- rowsAboveThreshold[i,]
        id <- row$id
        sulfate <- allData[which(allData$ID == id), which(names(allData) == "sulfate")]
        nitrate <- allData[which(allData$ID == id), which(names(allData) == "nitrate")]
        newRow <- cor(sulfate, nitrate, use="pairwise.complete.obs")
        answer <- c(answer, newRow)
    }
    answer
}