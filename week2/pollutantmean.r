pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  data_dir <- paste(getwd(), directory, sep = "/")
  files <- dir(data_dir, pattern = '\\.csv', full.names = TRUE)
  data <- 
    do.call("rbind", 
            lapply(files[c(id)], 
                   function(x) 
                     read.csv(file = x, header = TRUE)))
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  mean(data[,pollutant], na.rm = TRUE)
}