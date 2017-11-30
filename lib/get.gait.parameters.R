
setwd("~/code/masterthesis/thesis_R/")

erase.commas.at.the.ending <- function(filename){
  lines <- readLines(filename)
  library(stringr)
  for (i in 1:length(lines)){
    lines[i] <- str_replace(lines[i], ",$", "")
  }
  writeLines(lines, filename)
  return(NULL)
}

get.timestamps.from.strings <- function(dd){
  op <- options(digits.secs = 4)
  options(op)
  formatted.string.array <- paste(
    substr(dd, 1, 4), "/",    # YEAR
    substr(dd, 5,6), "/",     # MONTH
    substr(dd, 7,8), " ",     # DAY
    substr(dd, 9,10), ":",    # HOUR
    substr(dd, 11, 12), ":",  # MINUTES
    substr(dd, 13, 14), ".",  # SECONDS
    substr(dd, 15, 17),       # FRACTION
    sep = "")
  posix.datetime <- strptime(formatted.string.array, "%Y/%m/%d %H:%M:%OS")
  return(posix.datetime)
}
#################################################################
# data.folder <- "data/app"

filename <- "~/code/masterthesis/thesis_R/data/app/classiffication20171129T220051.csv"
erase.commas.at.the.ending(filename)

d <- read.csv(filename, colClasses = c("character", NA))
d[["timestamp"]] <- get.timestamps.from.strings(d[["timestamp"]])







