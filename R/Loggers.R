# @file Loggers.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
  registerLogger(createLogger(name = "DEFAULT",
                              threshold = "TRACE",
                              appenders = list(createFileAppender(layout = layoutParallel,
                                                                  fileName = fileName))))
}
