library(testthat)
#
# test_that("check shiny launcher",{
#   logFile <- tempfile()
#   registerLogger(createLogger(name = "TEST",
#                               threshold = "TRACE",
#                               appenders = list(createFileAppender(layout = layoutParallel,
#                                                                   fileName = logFile))))
#   logInfo("Hello world")
#   expect_error(expect_warning(launchLogViewer("nonExistentFile")))
# })
#
# # test_that("ensure installed functions correctly",{
# #   expect_message(ensure_installed("notARealPackage"))
# # })
