library(tidyverse)

read_my_file <- function(directory, id) {
  filename <- if (id < 10) {
    paste("0", "0", id, ".csv", sep="")
  } else if (id < 100) {
    paste("0", id, ".csv", sep="")
  } else
    paste(id, ".csv", sep="")
  return(read.csv(file.path(directory, filename),
                  colClasses = c("character", "numeric", "numeric", 
                               "numeric"), na.strings = "NA"))
}

get_col_id <- function(pollutant) {
  col <- if (pollutant == "sulfate") {
    2
  } else {
    3
  }
  return(col)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  col <- get_col_id(pollutant)
  
  data = data.frame()
  for (i in id[1]:id[length(id)]) {
    tmp <- read_my_file(directory, i)
    data <- rbind(data, tmp)
  }
  mean(data[, col], na.rm=TRUE)
}

complete <- function(directory, id = 1:332) {
  result = data.frame()

  for (i in id) {
    tmp <- read_my_file(directory, i)
    count <- sum(!is.na(tmp[, c("sulfate")]) & !is.na(tmp[, c("nitrate")]))
    result <- rbind(result, c(i, count))
  }
  names(result) <- c("id", "nobs")
  return(result)
  
}

corr <- function(directory, threshold = 0) {
  correlations <- numeric()
  complete_data <- complete(directory)
  for (i in 1:332) {
    tmp <- read_my_file(directory, i)
    if (complete_data[complete_data$id==i, ]$nobs > threshold) {
      correlations <- c(correlations, cor(tmp$sulfate, tmp$nitrate, 
                                          use = "pairwise.complete.obs"))
    }
  }
  return(correlations)
}