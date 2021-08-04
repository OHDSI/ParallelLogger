library("testthat")

test_that("Creation of args functions", {
  argsFun <- createArgFunction("createLogger")
  expect_true(any(grepl("#' @param", argsFun)))
  expect_true(any(grepl("createCreateLoggerArgs <- function", argsFun)))
})
