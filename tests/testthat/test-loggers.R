library("testthat")

test_that("Creating a logger", {
  expect_error(createLogger(
    name = "TEST",
    threshold = "TRACE",
    appenders = "list()"
  ))
  logFile <- tempfile()
  logger <- createLogger(
    name = "TEST",
    threshold = "TRACE",
    appenders = list(createFileAppender(
      layout = layoutParallel,
      fileName = logFile
    ))
  )
  expect_s3_class(logger, "Logger")
  expect_identical(logger$name, "TEST")
})

test_that("create default console logger", {
  defLogger <- addDefaultConsoleLogger(name = "defaultTest")
  expect_identical(defLogger$name, "defaultTest")
  expect_identical(defLogger$threshold, "INFO")
  expect_true(unregisterLogger("defaultTest"))
})

test_that("test def file logger", {
  logFile <- tempfile()
  tempFileDefLog <- addDefaultFileLogger(name = "defaultFileTest", fileName = logFile)
  logTrace("Hello world")
  log <- readChar(logFile, file.info(logFile)$size)
  expect_true(grepl("Hello world", log))
  expect_true(unregisterLogger("defaultFileTest"))
  unlink(logFile)
})

test_that("test the default email logger", {
  mailSettings <- list(
    from = "someone@gmail.com",
    to = "someone_else@gmail.com",
    engine = "curl",
    engineopts  = list(
      username = "someone@gmail.com",
      password = "Secret!"
    ), 
    control = list(
      host.name = "smtp.gmail.com:587"
    )
  )
  addDefaultEmailLogger(mailSettings, "My R session", test = TRUE)
  output <- capture.output(logFatal("Something bad"))
  expect_true(any(grepl("You've got mail:", output)))
  expect_true(unregisterLogger("DEFAULT_EMAIL_LOGGER"))
})

test_that("test email logger throws error when using mailR settings", {
  mailSettings <- list(
    from = "someone@gmail.com",
    to = c("someone_else@gmail.com"),
    smtp = list(
      host.name = "smtp.gmail.com",
      port = 465,
      user.name = "someone@gmail.com",
      passwd = "super_secret!",
      ssl = TRUE
    ),
    authenticate = TRUE,
    send = TRUE
  )

  expect_error(addDefaultEmailLogger(mailSettings), "mailR")
})

test_that("test default error report logger", {
  logFile <- tempfile()
  addDefaultErrorReportLogger(fileName = logFile, name = "temp default error")
  logInfo("Hello World")
  logFatal("ERRORERRORERROR")
  log <- readChar(logFile, file.info(logFile)$size)
  expect_false(grepl("Hello world", log))
  expect_true(grepl("ERRORERRORERROR", log))
  expect_true(unregisterLogger("temp default error"))
  unlink(logFile)
})
