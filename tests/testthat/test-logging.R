library("testthat")

test_that("Logging to file", {
  logFile <- tempfile()
  registerLogger(createLogger(name = "TEST",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile))))
  logInfo("Hello world")
  log <- readChar(logFile, file.info(logFile)$size)
  expect_true(grepl("Hello world", log))
  expect_true(unregisterLogger("TEST"))
  unlink(logFile)
})
