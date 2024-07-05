# Unit tests that won't run in R's testing framework

library(testthat)
library(ParallelLogger)

# Main thread message, warning, error logging ----------------------------------
logFile <- tempfile()
registerLogger(createLogger(name = "TEST",
                            threshold = "TRACE",
                            appenders = list(createFileAppender(layout = layoutParallel,
                                                                fileName = logFile))))
message("A message")
warning("A warning")
stop("A fatal error")

rlang::inform("An rlang message")
rlang::warn("An rlang warning")
rlang::abort("An rlang fatal error")

log <- readChar(logFile, file.info(logFile)$size)
expect_true(grepl("A message", log))
expect_true(grepl("A warning", log))
expect_true(grepl("A fatal error", log))
expect_true(grepl("An rlang message", log))
expect_true(grepl("An rlang warning", log))
expect_true(grepl("An rlang fatal error", log))
unlink(logFile)
expect_true(unregisterLogger("TEST"))

# Multi-thread message, warning, error logging ---------------------------------
logFile <- tempfile()
registerLogger(createLogger(name = "TEST",
                            threshold = "TRACE",
                            appenders = list(createFileAppender(layout = layoutParallel,
                                                                fileName = logFile))))
fun <- function(x) {
  message("Message ", x)
  warning("Warning ", x)
  stop("Fatal error ", x)
  
}
cluster <- makeCluster(3)
invisible(clusterApply(cluster, 1:3, fun))
stopCluster(cluster)

log <- readChar(logFile, file.info(logFile)$size)
expect_true(grepl("Message 1", log))
expect_true(grepl("Warning 1", log))
expect_true(grepl("Fatal error 1", log))
expect_true(grepl("Message 2", log))
expect_true(grepl("Warning 2", log))
expect_true(grepl("Fatal error 2", log))
expect_true(grepl("Message 3", log))
expect_true(grepl("Warning 3", log))
expect_true(grepl("Fatal error 3", log))
unlink(logFile)
expect_true(unregisterLogger("TEST"))

# Multi-logger message, warning, error logging ---------------------------------
logFile1 <- tempfile()
registerLogger(createLogger(name = "TEST1",
                            threshold = "TRACE",
                            appenders = list(createFileAppender(layout = layoutParallel,
                                                                fileName = logFile1))))
logFile2 <- tempfile()
registerLogger(createLogger(name = "TEST2",
                            threshold = "TRACE",
                            appenders = list(createFileAppender(layout = layoutParallel,
                                                                fileName = logFile2))))
message("A message")
warning("A warning")
stop("A fatal error")

log1 <- readChar(logFile1, file.info(logFile1)$size)
expect_true(grepl("A message", log1))
expect_true(grepl("A warning", log1))
expect_true(grepl("A fatal error", log1))
log2 <- readChar(logFile2, file.info(logFile2)$size)
expect_true(grepl("A message", log2))
expect_true(grepl("A warning", log2))
expect_true(grepl("A fatal error", log2))
unlink(logFile1)
unlink(logFile2)
expect_true(unregisterLogger("TEST1"))
expect_true(unregisterLogger("TEST2"))

# Logging message, warning, error in R6 objects ----------------------------------------------------
logFile1 <- tempfile()
registerLogger(createLogger(name = "TEST1",
                            threshold = "TRACE",
                            appenders = list(createFileAppender(layout = layoutParallel,
                                                                fileName = logFile1))))
logFile2 <- tempfile()
myClass <- R6::R6Class(
  public = list(
    run = function(logFile2) {
      registerLogger(createLogger(name = "TEST2",
                                  threshold = "TRACE",
                                  appenders = list(createFileAppender(layout = layoutParallel,
                                                                      fileName = logFile2))))
      message("A message")
      warning("A warning")
      unregisterLogger("TEST2")
    }
  )
)

myObject <- myClass$new()
myObject$run(logFile2)

log1 <- readChar(logFile1, file.info(logFile1)$size)
expect_true(grepl("A message", log1))
expect_true(grepl("A warning", log1))
log2 <- readChar(logFile2, file.info(logFile2)$size)
expect_true(grepl("A message", log2))
expect_true(grepl("A warning", log2))
unlink(logFile1)
unlink(logFile2)
expect_true(unregisterLogger("TEST1"))


