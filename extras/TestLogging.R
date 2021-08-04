# Unit tests that won't run in R's testing framework

library(testthat)

# Logging warning and stop ----------------------------------
logFile <- tempfile()
registerLogger(createLogger(name = "TEST",
                            threshold = "TRACE",
                            appenders = list(createFileAppender(layout = layoutParallel,
                                                                fileName = logFile))))
warning("Hello world")
stop("Hello again")

log <- readChar(logFile, file.info(logFile)$size)
expect_true(grepl("Hello world", log))
expect_true(grepl("Hello again", log))

unlink(logFile)
expect_true(unregisterLogger("TEST"))
