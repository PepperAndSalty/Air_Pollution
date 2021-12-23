####################Coursera Programming Assignment 1####################
#########################################################################

# Part 1

## Main
pollutantmean <- function(directory, pollutant, id = 1:332) {
  mypath <- paste(getwd(),"rprog_data_specdata", directory, sep = "/")
  mylt <- list.files(mypath, full.names = TRUE)
  
  df <- data.frame()
  for(i in id) {
    data <- read.csv(mylt[i])
    df <- rbind(df, data)
  }
  
  mean(df[[pollutant]], na.rm = TRUE)
}

## Alternative - returns wrong value, why?
pollutantmean <- function(directory, pollutant, id = 1:332) {
  mypath <- paste(getwd(),"rprog_data_specdata", directory, sep = "/")
  mylt <- list.files(mypath, full.names = TRUE)
  
  means <- c()
  for(i in id) {
    mymean <- mean(read.csv(mylt[i])[[pollutant]], na.rm = TRUE)
    means <- c(means, mymean)
  }
  
  mean(means)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


# Part 2
library(tidyverse)

complete <- function(directory, id = 1:332) {
  mypath <- paste(getwd(),"rprog_data_specdata", directory, sep = "/")
  mylt <- list.files(mypath, full.names = TRUE)
  
  df <- data.frame()
  
  for(i in id){
    data <- read.csv(mylt[i])
    mysum <- sum(complete.cases(rowwise(data)))
    data <- data.frame(id = i, nobs = mysum)
    df <- rbind(df, data)
  }
  df
}
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

# Part 3
corr <- function(directory, threshold = 0){
  mypath <- paste(getwd(),"rprog_data_specdata", directory, sep = "/")
  mylt <- list.files(mypath, full.names = TRUE)
  
  mycorrs <- c()
  for(i in seq_along(mylt)){
    data <- read.csv(mylt[i])
    mysum <- sum(complete.cases(rowwise(data)))
    if(mysum >= threshold) {
      mycor <- cor(data$sulfate, data$nitrate, use = "complete.obs")
      mycorrs <- c(mycorrs, mycor)
    }
  }
  mycorrs
}

cr <- corr("specdata", 150)