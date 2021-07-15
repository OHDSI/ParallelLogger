# @file Appenders.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
    # Avoid note in check:
    missing(this)
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
#' @param layout           The layout to be used by the appender.
#' @param fileName         The name of the file to write to.
#' @param overwrite        Overwrite the file if it is older than the expiration time?
#' @param expirationTime   Expiration time in seconds
#'
#' @export
createFileAppender <- function(layout = layoutParallel,
                               fileName,
                               overwrite = FALSE,
                               expirationTime = 60) {
  appendFunction <- function(this, level, message) {
    # Avoid note in check:
    missing(level)
    tryCatch({
      suppressWarnings({
        if (overwrite && file.exists(fileName) && difftime(Sys.time(),
                                                           file.mtime(fileName),
                                                           units = "secs") > expirationTime) {
          open <- "wt"
        } else {
          open <- "at"
        }
        con <- file(fileName, open = open, blocking = FALSE)
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
      warning("Error '",
              e$message,
              "' when writing log to file '",
              fileName,
              ". Removing file appender from logger.")
    })
    if (is.null(getOption("threadNumber")) && identical(layout, layoutErrorReport)) {
      writeLines(paste("An error report has been created at ", fileName))
    }
  }
  appender <- list(appendFunction = appendFunction,
                   layout = layout,
                   fileName = fileName,
                   overwrite = overwrite,
                   expirationTime = expirationTime)
  class(appender) <- "Appender"
  return(appender)
}

#' Create e-mail appender
#'
#' @details
#' Creates an appender that will send log events to an e-mail address using the \code{mailR} package.
#' Please make sure your settings are correct by using the mailR package before using those settings
#' here. ParallelLogger will not display any messages if something goes wrong when sending the e-mail.
#'
#' @param layout         The layout to be used by the appender.
#' @param mailSettings   Arguments to be passed to the send.mail function in the mailR package (except
#'                       subject and body).
#' @param label          A label to be used in the e-mail subject to identify a run. By default the
#'                       name of the computer is used.
#' @param test           If TRUE, a message will be displayed on the console instead of sending an
#'                       e-mail.
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
#' # Setting test to TRUE in this example so we don't really send an e-mail:
#' appender <- createEmailAppender(layout = layoutEmail,
#'                                 mailSettings = mailSettings,
#'                                 label = "My R session",
#'                                 test = TRUE)
#'
#' logger <- createLogger(name = "EMAIL", threshold = "FATAL", appenders = list(appender))
#' registerLogger(logger)
#'
#' logFatal("Something bad")
#'
#' unregisterLogger("EMAIL")
#'
#' @export
createEmailAppender <- function(layout = layoutEmail,
                                mailSettings,
                                label = Sys.info()["nodename"],
                                test = FALSE) {
  ensure_installed("mailR")
  
  appendFunction <- function(this, level, message) {
    # Avoid note in check:
    missing(this)
    
    # No need to send an e-mail at the death of an orphan thread:
    testString <- "Error in unserialize(node$con)"
    if (grepl(testString, message)[[1]]) {
      return()
    }
    
    # Only main thread gets to send e-mails:
    if (!is.null(getOption("threadNumber"))) {
      return()
    }
    
    mailSettings$subject <- sprintf("[%s] %s", label, level)
    mailSettings$body <- message
    if (test) {
      myfun <- testSendMail
    } else {
      myfun <- get("send.mail", asNamespace("mailR"))
    }
    try(do.call(myfun, mailSettings), silent = TRUE)
  }
  appender <- list(appendFunction = appendFunction,
                   layout = layout,
                   mailSettings = mailSettings,
                   label = label)
  class(appender) <- "Appender"
  return(appender)
}

testSendMail <- function(to, subject, body, ...) {
  writeLines("You've got mail:")
  writeLines("To:")
  writeLines(to)
  writeLines("")
  writeLines("Subject:")
  writeLines(subject)
  writeLines("")
  writeLines("Body:")
  writeLines(body)
}
