complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  data_dir <- paste(getwd(), directory, sep = "/")
  files <- dir(data_dir, pattern = '\\.csv', full.names = TRUE)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  data <- 
    do.call("rbind", 
            lapply(files[c(id)], 
                   function(x) 
                     read.csv(file = x, header = TRUE)))
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nobs <- complete.cases( data$sulfate , data$nitrate )
  aggregate( nobs ~ ID , data = data , FUN = sum )
}