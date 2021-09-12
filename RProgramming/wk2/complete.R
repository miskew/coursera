complete <- function (directory, id = 1:332) {
    file_list <- list.files(directory, full.names = TRUE)
    df <- data.frame()
    for (i in id) {
        df <- rbind(df, read.csv(file_list[i]))
    }

    # Throw away all rows with missing values
    df <- df[complete.cases(df),]

    # Build data frame with id - number of objects
    tallies <- data.frame()
    for (i in id) {
        matchingRowsForGivenId <- df[which(df$ID == i),]
        tallies <- rbind(tallies, c(i, dim(matchingRowsForGivenId)[1]))
    }
    names(tallies) <- c("id", "nobs")
    tallies
}
