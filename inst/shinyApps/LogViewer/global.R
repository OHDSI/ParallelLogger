# logFileName <- "S:/ScyllaEstimation/MetaAnalysis/log.txt"
# logFileName <- "S:/Covid19SusceptibilityAlphaBlockersTest/log.txt"

fileConnection <- file(logFileName, open = "r")
onStop(function() {close(fileConnection); message("File connection closed")})

batchSize <- 1000

levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
colorLevels <- c("TRACE", "DEBUG", "WARN", "ERROR", "FATAL")
colors <- c(rgb(0.8, 0.9, 1), rgb(0.8, 1, 0.8), rgb(1.0, 0.88, 0.7), rgb(1, 0.84, 0.8), rgb(1, 0.8, 0.94))
