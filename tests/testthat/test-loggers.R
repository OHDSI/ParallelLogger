library("testthat")

test_that("Creating a logger", {
  expect_error(createLogger(name = "TEST",
                            threshold = "TRACE",
                            appenders = "list()"))
  logFile <- tempfile()
  logger <- createLogger(name = "TEST",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = logFile)))
  expect_identical(class(logger), "Logger")
  expect_identical(logger$name, 'TEST')
})

test_that("create default console logger", {
  defLogger <- addDefaultConsoleLogger(name = "defaultTest")
  expect_identical(defLogger$name, "defaultTest")
  expect_identical(defLogger$threshold, "INFO")
  unregisterLogger("defaultTest")
})

test_that("test def file logger",{
  logFile <- tempfile()
  tempFileDefLog <- addDefaultFileLogger(name = "defaultFileTest", fileName = logFile)
  logTrace("Hello world")
  log <- readChar(logFile, file.info(logFile)$size)
  expect_true(grepl("Hello world", log))
  expect_true(unregisterLogger("defaultFileTest"))
  expect_error(addDefaultFileLogger(fileName = badName))
})

test_that("test the default email logger", {
  expect_error(addDefaultEmailLogger(mailSettings, "My R session", test = TRUE))
  # mailSettings <- list(from = "someone@gmail.com",
  #                      to = c("someone_else@gmail.com"),
  #                      smtp = list(host.name = "smtp.gmail.com",
  #                                  port = 465,
  #                                  user.name = "someone@gmail.com",
  #                                  passwd = "super_secret!",
  #                                  ssl = TRUE),
  #                      authenticate = TRUE,
  #                      send = TRUE)
  # 
  # # Setting test to TRUE in this example so we don't really send an e-mail:
  # addDefaultEmailLogger(mailSettings, "My R session", test = TRUE)
  # logFatal("Something bad")
  # stop("something bad")
  # unregisterLogger("DEFAULT_EMAIL_LOGGER")
})

test_that("test default error logger",{
  logFile <- tempfile()
  addDefaultErrorReportLogger(fileName = logFile, name = "temp default error")
  logInfo("Hello World")
  logFatal("ERRORERRORERROR")
  log <- readChar(logFile, file.info(logFile)$size)
  expect_false(grepl("Hello world", log))
  expect_true(grepl("ERRORERRORERROR", log))
  unregisterLogger("temp default error")
})

