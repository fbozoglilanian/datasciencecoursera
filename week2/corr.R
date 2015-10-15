source("complete.R")
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  data_dir <- paste(getwd(), directory, sep = "/")
  files <- dir(data_dir, pattern = '\\.csv', full.names = TRUE)
  comp <- complete(directory)
  c <- subset(comp, nobs > threshold)
  id <- c[, "ID"]
  
  data <- 
    do.call("rbind", 
            lapply(files[c(id)], 
                   function(x) {
                     d <- read.csv(file = x, header = TRUE)
                     d <- d[!is.na(d$nitrate) & !is.na(d$sulfate), ]
                     cor(d$nitrate, d$sulfat, use = "everything")
                   }))
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  as.vector(data)
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}