complete <- function(directory, id = 1:332) {
  wd <- getwd()
  setwd(directory)
  files <- dir()
  flag <- 0
  l <- id
  for (i in l) {
    if (flag == 0) {
      flag <- 1
      file <- read.csv(files[i])
      good <- !is.na(file[,"sulfate"]) & !is.na(file[,"nitrate"])
      good2 <- good[good]
      id <- i
      nobs <- length(good2)
    }
    else{
      file <- read.csv(files[i])
      good <- !is.na(file[,"sulfate"]) & !is.na(file[,"nitrate"])
      good2 <- good[good]
      id <- c(id, i)
      nobs <- c(nobs, length(good2))
    }
  }
  setwd(wd)
  data.frame(id, nobs)
}

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases