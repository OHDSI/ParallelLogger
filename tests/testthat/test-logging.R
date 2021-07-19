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
ParallelLogger::clearLoggers()

test_that("Logging to file when log folder is deleted", {
  logFolder <- file.path(tempdir(), "logFolder")
  dir.create(logFolder)
  logFile <- file.path(logFolder, "log.txt")
  registerLogger(createLogger(name = "TEST",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile))))
  logInfo("Hello world")
  unlink(logFolder, recursive = TRUE)
  expect_warning(logInfo("Hello world"))
  expect_error(unregisterLogger("TEST"))
})
ParallelLogger::clearLoggers()

test_that("logging with bad logger", {
  expect_error(registerLogger("not a logger"))
})
ParallelLogger::clearLoggers()


test_that("unregistering logger", {
  clearLoggers()
  expect_false(unregisterLogger(2, silent = T))
  logFile <- tempfile()
  registerLogger(createLogger(name = "TEST",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile))))
  registerLogger(createLogger(name = "TEST2",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile))))
  registerLogger(createLogger(name = "TEST3",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile))))
  expect_false(unregisterLogger(4, silent = T))
  expect_warning(unregisterLogger(4))
  unregisterLogger(2)
  expect_identical(getLoggers()[[2]]$name, "TEST3")
  expect_true(unregisterLogger(1))
  loggerItself <- createLogger(name = "TEST3",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile)))
  registerLogger(loggerItself)
  loggerTest <- getLoggers()[[1]]
  expect_true(unregisterLogger(loggerTest))
  expect_warning(unregisterLogger(list(1,2,"a")))
  expect_false(unregisterLogger(list(1,2,"a"), silent = T))
})
ParallelLogger::clearLoggers()

test_that("testing level to int",{
  expect_equal(levelToInt("TRACE"),1)
  expect_equal(levelToInt("DEBUG"),2)
  expect_equal(levelToInt("INFO"),3)
  expect_equal(levelToInt("WARN"),4)
  expect_equal(levelToInt("ERROR"),5)
  expect_equal(levelToInt("FATAL"),6)
  expect_error(levelToInt("notALevel"))
})

test_that("different logLevel functions",{
  logFile <- tempfile()
  registerLogger(createLogger(name = "TEST",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile))))

  logTrace("this is a TRACE")
  logDebug("this is a DEBUG")
  logInfo("this is an INFO")
  logWarn("this is a WARN")
  logError("this is an ERROR")
  logFatal("this is FATAL")
  log <- readChar(logFile, file.info(logFile)$size)
  expect_equal(stringr::str_count(log, "TRACE"), 2)
  expect_equal(stringr::str_count(log, "DEBUG"), 2)
  expect_equal(stringr::str_count(log, "INFO"), 2)
  expect_equal(stringr::str_count(log, "WARN"), 2)
  expect_equal(stringr::str_count(log, "ERROR"), 2)
  expect_equal(stringr::str_count(log, "FATAL"), 2)
})