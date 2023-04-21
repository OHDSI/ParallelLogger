# Unit tests that won't run in R's testing framework

library(testthat)
library(ParallelLogger)

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
# writeLines(log)
expect_true(grepl("A message", log))
expect_true(grepl("A warning", log))
expect_true(grepl("A fatal error", log))
expect_true(grepl("An rlang message", log))
expect_true(grepl("An rlang warning", log))
expect_true(grepl("An rlang fatal error", log))

# launchLogViewer(logFile)
unlink(logFile)
expect_true(unregisterLogger("TEST"))
