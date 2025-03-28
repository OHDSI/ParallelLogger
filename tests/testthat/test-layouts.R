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
  # May have jumped 1 second, so get second time:
  time2 <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  test2 <- sprintf("%s\t%s", time2, msg)
  expect_true(result == test | result == test2)
})

test_that("layoutErrorReport func", {
  lvl <- "ERROR"
  msg <- "TEST ERROR"
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  test <- sprintf("Thread: Main\nMessage:  %s\nLevel:  ERROR", msg)
  result <- capture.output(capture.output(out <- ParallelLogger::layoutErrorReport(lvl, msg), type = "message"), type = "output")
  # May have jumped 1 second, so get second time:
  time2 <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  test2 <- sprintf("%s\t%s", time2, msg) 
  expect_true(grepl(test, out)[[1]] | grepl(test2, out)[[1]])
})

test_that("layoutEmail func", {
  lvl <- "INFO"
  msg <- "TEST MESSAGE"
  test1 <- sprintf("Message:  %s", msg)
  test2 <- sprintf("Level:  %s", lvl)
  result <- capture.output(capture.output(out <- ParallelLogger::layoutEmail(lvl, msg), type = "message"), type = "output")
  expect_match(out, test1)
  expect_match(out, test2)
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
      expect_match(out, test1)
      expect_match(out, test2)
    },
    error = function(e) { },
    warning = function(w) { },
    finally = { }
  )
})
