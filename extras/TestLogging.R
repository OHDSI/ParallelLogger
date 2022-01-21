# Unit tests that won't run in R's testing framework

library(testthat)

# Logging warning and stop ----------------------------------
logFile <- tempfile()
registerLogger(createLogger(name = "TEST",
                            threshold = "TRACE",
                            appenders = list(createFileAppender(layout = layoutParallel,
                                                                fileName = logFile))))
message("Hi!")
warning("Hello world")
stop("Hello again")

rlang::inform("Alpha")
rlang::warn("Beta")
rlang::abort("Gamma")

log <- readChar(logFile, file.info(logFile)$size)
writeLines(log)
expect_true(grepl("Hi!", log))
expect_true(grepl("Hello world", log))
expect_true(grepl("Hello again", log))
expect_true(grepl("Alpha", log))
expect_true(grepl("Beta", log))
expect_true(grepl("Gamma", log))

launchLogViewer(logFile)


unlink(logFile)
expect_true(unregisterLogger("TEST"))
