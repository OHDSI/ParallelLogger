# @file Loggers.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of ParallelLogger
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create a logger
#'
#' @details
#' Creates a logger that will log messages to its appenders. The logger will only log messages at a
#' level equal to or higher than its threshold. For example, if the threshold is "INFO" then messages
#' marked "INFO" will be logged, but messages marked "TRACE" will not. The order of levels is "TRACE",
#' "DEBUG", "INFO", "WARN", "ERROR, "and FATAL".
#'
#' @param name        A name for the logger.
#' @param threshold   The threshold to be used for reporting.
#' @param appenders   A list of one or more appenders as created for example using the
#'                    \code{\link{createConsoleAppender}} or \code{\link{createFileAppender}} function.
#'
#' @return
#' An object of type \code{Logger}, to be used with the \code{\link{registerLogger}} function.
#' 
#' @template LoggingExample
#'
#' @export
createLogger <- function(name = "SIMPLE",
                         threshold = "INFO",
                         appenders = list(createConsoleAppender())) {
  for (appender in appenders) if (!is(appender, "Appender"))
    stop("All appenders must be of class 'Appender'")
  logFunction <- function(this, level, message) {
    for (appender in this$appenders) {
      formatted <- appender$layout(level, message)
      appender$appendFunction(appender, level, formatted)
    }
  }
  logger <- list(name = name,
                 logFunction = logFunction,
                 threshold = threshold,
                 appenders = appenders)
  class(logger) <- "Logger"
  return(logger)
}

#' Add the default console logger
#'
#' @details
#' Creates a logger that writes to the console using the "INFO" threshold and the
#' \code{\link{layoutSimple}} layout.
#'
#' @examples 
#' logger <- addDefaultConsoleLogger()
#' logTrace("This event is below the threshold (INFO)")
#' logInfo("Hello world")                       
#' unregisterLogger(logger)  
#' 
#' @export
addDefaultConsoleLogger <- function() {
  logger <- createLogger()
  registerLogger(logger)
  invisible(logger)
}

#' Add the default file logger
#'
#' @details
#' Creates a logger that writes to a file using the "TRACE" threshold and the
#' \code{\link{layoutParallel}} layout. The output can be viewed with the built-in log viewer that can
#' be started using \code{\link{launchLogViewer}}.
#'
#' @param fileName   The name of the file to write to.
#'
#' @export
addDefaultFileLogger <- function(fileName) {
  registerLogger(createLogger(name = "DEFAULT_FILE_LOGGER",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = fileName))))
}

#' Add the default e-mail logger
#'
#' @details
#' Creates a logger that writes to e-mail using the "FATAL" threshold and the
#' \code{\link{layoutEmail}} layout. This function uses the \code{mailR} package. Please
#' make sure your e-mail settings are correct by using the mailR package before using those settings here. 
#' ParallelLogger will not display any messages if something goes wrong when sending the e-mail.
#'
#' @param mailSettings    Arguments to be passed to the send.mail function in the mailR package (except
#'                        subject and body).
#' @param label           A label to be used in the e-mail subject to identify a run. By default the
#'                        name of the computer is used.
#' @param test            If TRUE, a message will be displayed on the console instead of sending an e-mail.
#' 
#' @examples
#' mailSettings <- list(from = "someone@gmail.com",
#'                      to = c("someone_else@gmail.com"),
#'                      smtp = list(host.name = "smtp.gmail.com",
#'                                  port = 465,
#'                                  user.name = "someone@gmail.com",
#'                                  passwd = "super_secret!",
#'                                  ssl = TRUE),
#'                      authenticate = TRUE,
#'                      send = TRUE)
#'                      
#' # Setting test to TRUE in this example so we don't really send an e-mail:
#' addDefaultEmailLogger(mailSettings, "My R session", test = TRUE)
#' logFatal("Something bad")
#' 
#' unregisterLogger("DEFAULT")
#'
#' @export
addDefaultEmailLogger <- function(mailSettings, label = Sys.info()["nodename"], test = FALSE) {
  registerLogger(createLogger(name = "DEFAULT_EMAIL_LOGGER",
                              threshold = "FATAL",
                              appenders = list(createEmailAppender(layout = layoutEmail,
                                                                   mailSettings = mailSettings,
                                                                   label = label,
                                                                   test = test))))
}

#' Add the default error report logger
#'
#' @details
#' Creates a logger that writes to a file using the "FATAL" threshold and the
#' \code{\link{layoutErrorReport}} layout. The file will be overwritten if it is 
#' older than 60 seconds. The user will be notified that the error report has been
#' created, and where to find it.
#'
#' @param fileName   The name of the file to write to.
#'
#' @export
addDefaultErrorReportLogger <- function(fileName = file.path(getwd(), "errorReportR.txt")) {
  registerLogger(createLogger(name = "DEFAULT_ERRORREPORT_LOGGER",
                              threshold = "FATAL",
                              appenders = list(createFileAppender(layout = layoutErrorReport,
                                                                  fileName = fileName,
                                                                  overwrite = TRUE,
                                                                  expirationTime = 60)))) 
}
