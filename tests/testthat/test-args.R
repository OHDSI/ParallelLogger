library("testthat")

test_that("Creation of args functions", {
  argsFun <- createArgFunction("createLogger")
  expect_true(any(grepl("#' @param", argsFun)))
  expect_true(any(grepl("createCreateLoggerArgs <- function", argsFun)))
})


test_that("SelectFromList", {
  x <- list(
    a = list(name = "John", age = 25, gender = "M"),
    b = list(name = "Mary", age = 24, gender = "F")
  )
  selection <- selectFromList(x, c("name", "age"))
  expect_equal(length(selection), 2)  
  expect_true(all(c("name", "age") %in% names(selection[[1]])))
  expect_false("gender" %in% names(selection[[1]]))
})
