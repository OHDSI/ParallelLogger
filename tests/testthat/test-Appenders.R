# test nonsense email address
test_that("testsendmail correct", {
  to <- "bla"
  subject <- "testing this mail"
  body <- "Dear sir madam, I hope this test finds you well. Sincerely, John Doe"
  expect_error(testSendMail(to, subject, body))
})

# test if output is produced
test_that("testsendmail correct", {
  to <- "johndoe@gmail.com"
  subject <- "testing this mail"
  body <- "Dear sir madam, I hop this test finds you well. Sincerely, John Doe"
  expect_output(testSendMail(to, subject, body))
})
