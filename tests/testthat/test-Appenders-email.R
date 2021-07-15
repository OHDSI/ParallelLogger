#test nonsense emailaddress
test_that("testsendmail correct", {
  to <- "bla"
  subject <- "testing this mail"
  body <- "Dear sir madam, I hop this test finds you well. Sincerely, John Doe"
  expect_error(testSendMail(to, subject, body))
})