corr <- function(directory, threshold = 0) {
  wd <- getwd()
  setwd(directory)
  files <- dir()
  flag <- 0
  correl <- c()
  for (i in 1:332) {
      file <- read.csv(files[i])
      good <- !is.na(file[,"sulfate"]) & !is.na(file[,"nitrate"])
      g <- good[good]
      if (length(g)>threshold){
     #   correl <- c(correl,cor(file[good,"sulfate"],file[good,"nitrate"]))
        correl <- c(correl,cor(file[,"sulfate"],file[,"nitrate"], use = "complete.obs"))
      }
  }
  setwd(wd)
  correl
 }
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!