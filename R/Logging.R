# @file Logging.R
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

isRmdCheck <- function() {
  return(Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != "")
}

isUnitTest <- function() {
  return(tolower(Sys.getenv("TESTTHAT", "")) == "true")
}

registerDefaultHandlers <- function() {
  if (isRmdCheck() || isUnitTest()) {
    message("Either in Rmd Check or a unit test (or both). Not capturing errors and warnings in ParallelLogger")
    return(NULL)
  }
  previousErrorHandler <- getOption("error")
  
  logBaseError <- function() {
    logFatal(gsub("\n", " ", geterrmessage()))
    if (!is.null(previousErrorHandler)) {
      eval(previousErrorHandler)
    }
  }
  options(error = logBaseError)

  options(warning.expression = quote({
    evaluate <- function(message, frameIndex) {
      if (frameIndex < -20) {
        return(as.character(force(message)))
      } else {
        text <- tryCatch(as.character(eval(message, envir = sys.frame(frameIndex))),
                         error = function(x) "error")
        if (text == "error") {
          return(evaluate(message, frameIndex - 2))
        } else {
          return(text)
        }
      }
    }

    for (i in 1:sys.nframe()) {
      frame <- sys.call(-i)
      if (!is.null(frame) && length(frame) > 1) {
        name <- as.character(frame[[1]])
        if (length(name) == 1) {
          if (is.language(frame[[1]]) && name == "warning") {
          ParallelLogger::logWarn(evaluate(frame[[2]], -i - 1))
          break
          } else if (name == ".signalSimpleWarning") {
          ParallelLogger::logWarn(frame[[2]])
          break
          } else if (name == ".Deprecated") {
          ParallelLogger::logWarn("This function is deprecated. Use '", frame[[2]], "' instead.")
          break
          }
        }
      }
    }
  }))
}

getDefaultLoggerSettings <- function() {
  return(list(loggers = list(createLogger())))
}

getLoggerSettings <- function() {
  settings <- getOption("loggerSettings")
  if (is.null(settings)) {
    settings <- getDefaultLoggerSettings()
  }
  if (is.null(getOption("warning.expression"))) {
    registerDefaultHandlers()
  }
  return(settings)
}

setLoggerSettings <- function(settings) {
  options(loggerSettings = settings)
}

#' Register a logger
#'
#' @details
#' Registers a logger as created using the \code{\link{createLogger}} function to the logging system.
#'
#' @param logger   An object of type \code{Logger} as created using the \code{\link{createLogger}}
#'                 function.
#'
#' @template LoggingExample
#'
#' @export
registerLogger <- function(logger) {
  if (!is(logger, "Logger"))
    stop("Logger must be of class 'Logger'")
  settings <- getLoggerSettings()
  settings$loggers[[length(settings$loggers) + 1]] <- logger
  setLoggerSettings(settings)
  invisible(NULL)
}

#' Unregister a logger
#'
#' @details
#' Unregisters a logger from the logging system.
#'
#' @param x        Can either be an integer (e.g. 2 to remove the second logger), the name of the
#'                 logger, or the logger object itself.
#' @param silent   If TRUE, no warning will be issued if the logger is not found.
#'
#' @return
#' Returns TRUE if the logger was removed.
#'
#' @template LoggingExample
#'
#' @export
unregisterLogger <- function(x, silent = FALSE) {
  settings <- getLoggerSettings()
  if (is.integer(x) || is.numeric(x)) {
    if (x <= length(settings$loggers)) {
      settings$loggers[[x]] <- NULL
      setLoggerSettings(settings)
      return(TRUE)
    } else {
      if (!silent) {
        warning("Could not find logger ", x)
      }
      return(FALSE)
    }
  } else if (is.character(x)) {
    for (i in 1:length(settings$loggers)) {
      if (settings$loggers[[i]]$name == x) {
        settings$loggers[[i]] <- NULL
        setLoggerSettings(settings)
        return(TRUE)
      }
    }
    if (!silent) {
      warning("Could not find logger ", x)
    }
    return(FALSE)
  } else if (is(x, "Logger")) {
    for (i in length(settings$loggers):1) {
      if (all.equal(x, settings$loggers[[i]])) {
        settings$loggers[[i]] <- NULL
        setLoggerSettings(settings)
        return(TRUE)
      }
    }
  }
  if (!silent) {
    warning("Could not find logger ", x)
  }
  return(FALSE)
}

#' Get all registered loggers
#'
#' @return
#' Returns all registered loggers.
#'
#' @export
getLoggers <- function() {
  settings <- getLoggerSettings()
  return(settings$loggers)
}

#' Remove all registered loggers
#'
#' @export
clearLoggers <- function() {
  settings <- getLoggerSettings()
  settings$loggers <- list()
  setLoggerSettings(settings)
}


levelToInt <- function(level) {
  if (level == "TRACE")
    return(1)
  if (level == "DEBUG")
    return(2)
  if (level == "INFO")
    return(3)
  if (level == "WARN")
    return(4)
  if (level == "ERROR")
    return(5)
  if (level == "FATAL")
    return(6)
}

log <- function(level, ...) {
  message <- .makeMessage(...)
  settings <- getLoggerSettings()
  for (logger in settings$loggers) {
    if (levelToInt(level) >= levelToInt(logger$threshold)) {
      logger$logFunction(this = logger, level = level, message = message)
    }
  }
}

#' Log a message at the TRACE level
#'
#' @details
#' Log a message at the specified level. The message will be sent to all the registered loggers.
#'
#' @param ...   Zero or more objects which can be coerced to character (and which are pasted together
#'              with no separator).
#'
#' @template LoggingExample
#'
#' @export
logTrace <- function(...) {
  log(level = "TRACE", ...)
}

#' Log a message at the DEBUG level
#'
#' @details
#' Log a message at the specified level. The message will be sent to all the registered loggers.
#'
#' @param ...   Zero or more objects which can be coerced to character (and which are pasted together
#'              with no separator).
#'
#' @export
logDebug <- function(...) {
  log(level = "DEBUG", ...)
}

#' Log a message at the INFO level
#'
#' @details
#' Log a message at the specified level. The message will be sent to all the registered loggers.
#'
#' @param ...   Zero or more objects which can be coerced to character (and which are pasted together
#'              with no separator).
#'
#' @template LoggingExample
#'
#' @export
logInfo <- function(...) {
  log(level = "INFO", ...)
}

#' Log a message at the WARN level
#'
#' @details
#' Log a message at the specified level. The message will be sent to all the registered loggers. This
#' function is automatically called when a warning is thrown, and should not be called directly. Use
#' \code{warning()} instead.
#'
#' @param ...   Zero or more objects which can be coerced to character (and which are pasted together
#'              with no separator).
#'
#' @export
logWarn <- function(...) {
  log(level = "WARN", ...)
}

#' Log a message at the ERROR level
#'
#' @details
#' Log a message at the specified level. The message will be sent to all the registered loggers.
#'
#' @param ...   Zero or more objects which can be coerced to character (and which are pasted together
#'              with no separator).
#'
#' @export
logError <- function(...) {
  log(level = "ERROR", ...)
}

#' Log a message at the FATAL level
#'
#' @details
#' Log a message at the specified level. The message will be sent to all the registered loggers. This
#' function is be automatically called when an error occurs, and should not be called directly. Use
#' \code{stop()} instead.
#'
#' @param ...   Zero or more objects which can be coerced to character (and which are pasted together
#'              with no separator).
#'
#' @export
logFatal <- function(...) {
  log(level = "FATAL", ...)
}

