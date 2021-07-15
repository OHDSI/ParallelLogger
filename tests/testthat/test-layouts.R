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
