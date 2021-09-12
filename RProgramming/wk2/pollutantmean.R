pollutantmean <- function(directory, pollutant, id = 1:332) {
    file_list <- list.files(directory, full.names=TRUE)
    final <- data.frame()
    for (i in id) {
        final <- rbind(final, read.csv(file_list[i]))
    }
    data_subset <- final[pollutant]
    mean(data_subset[,], na.rm = TRUE)
}