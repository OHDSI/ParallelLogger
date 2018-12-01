# @file Appenders.R
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


#' Create console appender
#'
#' @details
#' Creates an appender that will write to the console.
#'
#' @param layout   The layout to be used by the appender.
#'
#' @template LoggingExample
#' 
#' @export
createConsoleAppender <- function(layout = layoutSimple) {
  appendFunction <- function(this, level, message) {
    if (level == "WARN" || level == "ERROR") {
      writeLines(message, con = stderr())
    } else if (level != "FATAL") {
      # Note: Fatal messages should originate from stop(), which will print its own message.
      writeLines(message, con = stdout())
    }
  }
  appender <- list(appendFunction = appendFunction, layout = layout)
  class(appender) <- "Appender"
  return(appender)
}

#' Create file appender
#'
#' @details
#' Creates an appender that will write to a file.
#'
#' @param layout     The layout to be used by the appender.
#' @param fileName   The name of the file to write to.
#'
#' @export
createFileAppender <- function(layout = layoutParallel, fileName) {
  appendFunction <- function(this, level, message) {
    tryCatch({
      suppressWarnings({
        con <- file(fileName, open = "at", blocking = FALSE)
        writeLines(text = message, con = con)
        flush(con)
        close(con)
      })
    }, error = function(e) {
      settings <- getLoggerSettings()
      for (i in length(settings$loggers):1) {
        for (j in length(settings$loggers[[i]]$appenders):1) {
          if (identical(settings$loggers[[i]]$appenders[[j]], this)) {
            settings$loggers[[i]]$appenders[[j]] <- NULL
            if (length(settings$loggers[[i]]$appenders) == 0) {
              settings$loggers[[i]] <- NULL
            }
          }
        }
      }
      setLoggerSettings(settings)
      warning("Error '", e$message, "' when writing log to file '", fileName, ". Removing file appender from logger.")
    })
  }
  appender <- list(appendFunction = appendFunction, layout = layout, fileName = fileName)
  class(appender) <- "Appender"
  return(appender)
}
