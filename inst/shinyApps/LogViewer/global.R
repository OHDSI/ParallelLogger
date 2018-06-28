# logFileName <- "c:/temp/logFile.txt"
eventLog <- read.table(logFileName, header = FALSE, sep = "\t")
colnames(eventLog) <- c("Timestamp", "Thread", "Level","Package", "Function", "Message")

levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")

colorLevels <- c("TRACE", "DEBUG", "WARN", "ERROR", "FATAL")
colors <- c(rgb(0.8, 0.9, 1), rgb(0.8, 1, 0.8), rgb(1.0, 0.88, 0.7), rgb(1, 0.84, 0.8), rgb(1, 0.8, 0.94))

threads <- as.character(unique(eventLog$Thread))
threads <- threads[order(threads)]
threads <- c("All", threads)
packages <- as.character(unique(eventLog$Package))
packages <- c("All", packages)