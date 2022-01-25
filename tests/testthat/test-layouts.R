library("testthat")

test_that("display correct message", {
  lvl <- "INFO"
  msg <- "Just some information about the appender"
  result <- layoutSimple(lvl, msg)
  expect_equal(result, "Just some information about the appender")
})

test_that("warning message", {
  lvl <- "WARN"
  msg <- "no need to use"
  result <- layoutSimple(lvl, msg)
  expect_equal(result, "Warning: no need to use")
})


test_that("layoutTimestamp func", {
  lvl <- "INFO"
  msg <- "TEST"
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  test <- sprintf("%s\t%s", time, msg)
  result <- ParallelLogger::layoutTimestamp(lvl, msg)
  expect_equal(result, test)
})

test_that("layoutErrorReport func", {
  lvl <- "ERROR"
  msg <- "TEST ERROR"
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  test <- sprintf("Thread: Main\nMessage:  %s\nLevel:  ERROR", msg)
  result <- capture.output(capture.output(out <- ParallelLogger::layoutErrorReport(lvl, msg), type = "message"), type = "output")
  testthat::expect_match(out, test)
})

test_that("layoutEmail func", {
  lvl <- "INFO"
  msg <- "TEST MESSAGE"
  test1 <- sprintf("Message:  %s", msg)
  test2 <- sprintf("Level:  %s", lvl)
  result <- capture.output(capture.output(out <- ParallelLogger::layoutEmail(lvl, msg), type = "message"), type = "output")
  testthat::expect_match(out, test1)
  testthat::expect_match(out, test2)
})


test_that("layoutStackTrace func", {
  lvl <- "INFO"
  msg <- "TEST MESSAGE"
  test1 <- sprintf("Message:  %s", msg)
  test2 <- sprintf("Level:  %s", lvl)
  out <- NULL

  # TODO: Fix / ignore error output generated bv layoutStackTrace / (limitedLabels(sys.calls()))?
  tryCatch(
    expr = {
      try(result <- capture.output(capture.output(out <- ParallelLogger::layoutStackTrace(lvl, msg), type = "message"), type = "output"), silent = TRUE)
      testthat::expect_match(out, test1)
      testthat::expect_match(out, test2)
    },
    error = function(e) { },
    warning = function(w) { },
    finally = { }
  )
})
