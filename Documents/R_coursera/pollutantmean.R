pollutantmean <- function(directory, pollutant, id=1:332) {
  wd <- getwd()
  setwd(directory)
  files <- dir()
  flag <- 0
  for (i in id) {
    file <- read.csv(files[i])
    good <- !is.na(file[,pollutant])
    if (flag==0){
      m <- file[good,pollutant]
      flag <-1
    }
    else{
      m <- c(m, file[good,pollutant])
    }
  }
  setwd(wd)
  mean(m)
}
