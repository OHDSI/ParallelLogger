# logFileName <- "c:/temp/logFile.txt"

# Fault-tolerant function for reading log files
readLog <- function(fileName) {
  con <- file(fileName, open = "r")
  lines <- readLines(con)
  close(con)
  rows <- strsplit(lines, "\t")
  malformed <- sapply(rows, function(x) length(x) != 6)
  rows <- rows[!malformed]
  result <- data.frame(Timestamp = as.Date(sapply(rows, function(x) x[1])),
                       Thread = sapply(rows, function(x) x[2]),
                       Level = sapply(rows, function(x) x[3]),
                       Package = sapply(rows, function(x) x[4]),
                       Function = sapply(rows, function(x) x[5]),
                       Message = sapply(rows, function(x) x[6]))
  return(result)
}
eventLog <- readLog(logFileName)
levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
colorLevels <- c("TRACE", "DEBUG", "WARN", "ERROR", "FATAL")
colors <- c(rgb(0.8, 0.9, 1), rgb(0.8, 1, 0.8), rgb(1.0, 0.88, 0.7), rgb(1, 0.84, 0.8), rgb(1, 0.8, 0.94))

threads <- as.character(unique(eventLog$Thread))
threads <- threads[order(threads)]
threads <- c("All", threads)
packages <- as.character(unique(eventLog$Package))
packages <- c("All", packages)


lines <- readChar(logFileName, file.info(logFileName)$size)




