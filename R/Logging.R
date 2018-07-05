# @file Logging.R
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

registerDefaultHandlers <- function() {
  logBaseError <- function() {
    logFatal(gsub("\n", " ", geterrmessage()))
  }
  options(error = logBaseError)

  options(warning.expression = quote(for (i in 1:sys.nframe()) {
    if (sys.call(-i)[[1]] == ".signalSimpleWarning" && length(sys.call(-i)) > 1) {
      ParallelLogger::logWarn(sys.call(-i)[[2]])
      break
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
    con <- file(fileName, open = "at", blocking = FALSE)
    writeLines(text = message, con = con)
    flush(con)
    close(con)
  }
  appender <- list(appendFunction = appendFunction, layout = layout, fileName = fileName)
  class(appender) <- "Appender"
  return(appender)
}

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
#' @param x   Can either be an integer (e.g. 2 to remove the second logger), the name of the logger, or
#'            the logger object itself.
#'
#' @return
#' Returns TRUE if the logger was removed.
#' 
#' @template LoggingExample
#'
#' @export
unregisterLogger <- function(x) {
  settings <- getLoggerSettings()
  if (is.integer(x) || is.numeric(x)) {
    if (x <= length(settings$loggers)) {
      settings$loggers[[x]] <- NULL
      setLoggerSettings(settings)
      return(TRUE)
    } else {
      warning("Could not find logger ", x)
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
    warning("Could not find logger ", x)
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
  warning("Could not find logger ", x)
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

#' Simple logging layout
#'
#' @description
#' A layout function to be used with an appender. This layout simply includes the message itself.
#'
#' @param level     The level of the message (e.g. "INFO")
#' @param message   The message to layout.
#' 
#' @export
layoutSimple <- function(level, message) {
  # Avoid check notes about non-used parameters:
  if (level == "WARN") {
    message <- paste("Warning:", message)
  }
  return(message)
}

#' Logging layout with timestamp
#'
#' @description
#' A layout function to be used with an appender. This layout adds the time to the message.
#'
#' @param level     The level of the message (e.g. "INFO")
#' @param message   The message to layout.
#'
#' @template LoggingExample
#' 
#' @export
layoutTimestamp <- function(level, message) {
  # Avoid check notes about non-used parameters:
  missing(level)
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sprintf("%s\t%s", time, message)
}

#' Logging layout for parallel computing
#'
#' @description
#' A layout function to be used with an appender. This layout adds the time, thread, level, package
#' name, and function name to the message.
#'
#' @param level     The level of the message (e.g. "INFO")
#' @param message   The message to layout.
#'
#' @export
layoutParallel <- function(level, message) {
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  threadNumber <- getOption("threadNumber")
  if (is.null(threadNumber)) {
    threadLabel <- "Main thread"
  } else {
    threadLabel <- paste("Thread", threadNumber)
  }
  functionName <- ""
  packageName <- ""
  if (sys.nframe() > 4) {
    for (i in 4:sys.nframe()) {
      packageName <- utils::packageName(env = sys.frame(-i))
      if (length(packageName) != 0 && packageName != "base" && packageName != "snow" && packageName !=
        "ParallelLogger") {
        if (class(sys.call(-i)[[1]]) == "function") {
          # USing do.call without quotes means the function name is lost
          functionName <- ""
        } else {
          functionName <- as.character(sys.call(-i)[[1]])
        }
        break
      }
    }
  }
  if (length(functionName) == 0) {
    functionName <- ""
  } else {
    functionName <- functionName[length(functionName)]
  }
  if (is.null(packageName)) {
    packageName <- ""
  }
  message <- gsub("\n", " ", message)
  sprintf("%s\t[%s]\t%s\t%s\t%s\t%s", time, threadLabel, level, packageName, functionName, message)
}

#' Logging layout with stacktrace
#'
#' @description
#' A layout function to be used with an appender. This layout adds the strack trace to the message.
#'
#' @param level     The level of the message (e.g. "INFO")
#' @param message   The message to layout.
#'
#' @export
layoutStackTrace <- function(level, message) {
  # Avoid check notes about non-used parameters:
  missing(level)
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  stackTrace <- c()
  nFrame <- -4
  fun <- sys.call(nFrame)
  while (!is.null(fun) && class(fun[[1]]) != "function") {
    stackTrace <- c(stackTrace, as.character(fun[[1]]))
    nFrame <- nFrame - 1
    fun <- sys.call(nFrame)
  }
  stackTrace <- paste(rev(stackTrace), collapse = " - ")
  sprintf("%s\t%s\t%s", time, stackTrace, message)
}
