# @file Layouts.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
  threadNumber <- getThreadNumber()
  if (threadNumber == 0) {
    threadLabel <- "Main thread"
  } else {
    threadLabel <- paste("Thread", threadNumber)
  }
  functionName <- ""
  packageName <- ""
  if (sys.nframe() > 4) {
    for (i in 4:sys.nframe()) {
      packageName <- utils::packageName(env = sys.frame(-i))
      if (length(packageName) != 0 && !packageName %in% c(
        "base",
        "snow",
        "ParallelLogger",
        "rlang"
      )) {
        if (is(sys.call(-i)[[1]], "function")) {
          # Using do.call without quotes means the function name is lost
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

#' Logging layout with stack trace
#'
#' @description
#' A layout function to be used with an appender. This layout adds the stack trace to the message.
#'
#' @param level     The level of the message (e.g. "INFO")
#' @param message   The message to layout.
#'
#' @export
layoutStackTrace <- function(level, message) {
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  threadNumber <- getThreadNumber()
  if (threadNumber == 0) {
    threadLabel <- "Main thread"
  } else {
    threadLabel <- paste("Thread", threadNumber)
  }
  trace < .tidyStackTrace(limitedLabels(sys.calls()))
  output <- paste(c(sprintf("%s\t[%s]\t%s\t%s", time, threadLabel, level, message),
    trace,
    collapse = "\n"
  ))
  return(output)
}

#' Logging layout for e-mail
#'
#' @description
#' A layout function to be used with an e-mail appender. This layout creates a short summary e-mail
#' message on the event, including stack trace.
#'
#' @param level     The level of the message (e.g. "INFO")
#' @param message   The message to layout.
#'
#' @export
layoutEmail <- function(level, message) {
  lines <- c()
  lines <- c(lines, paste("Message: ", message))
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  lines <- c(lines, paste("Time: ", time))
  lines <- c(lines, paste("Level: ", level))
  lines <- c(lines, "Stack trace:")
  lines <- c(lines, .tidyStackTrace(limitedLabels(sys.calls())))
  return(paste(lines, collapse = "\n"))
}

#' Logging layout for error report
#'
#' @description
#' A layout function to be used with an appender. This layout creates a more elaborate error message,
#' for sharing with the developer. If an error occurs in the main thread a summary of the system info
#' will be included.
#'
#' @param level     The level of the message (e.g. "INFO")
#' @param message   The message to layout.
#'
#' @export
layoutErrorReport <- function(level, message) {
  lines <- c()
  threadNumber <- getThreadNumber()
  if (threadNumber == 0) {
    lines <- c(lines, "Thread: Main")
  } else {
    lines <- c(lines, paste("Thread: ", threadNumber))
  }
  lines <- c(lines, paste("Message: ", message))
  lines <- c(lines, paste("Level: ", level))
  time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  lines <- c(lines, paste("Time: ", time))
  lines <- c(lines, "")
  lines <- c(lines, "Stack trace:")
  lines <- c(lines, .tidyStackTrace(limitedLabels(sys.calls())))
  lines <- c(lines, "")
  if (threadNumber == 0) {
    lines <- c(lines, .systemInfo())
    lines <- c(lines, "")
  }
  lines <- c(lines, "")
  return(paste(lines, collapse = "\n"))
}

.systemInfo <- function() {
  si <- sessionInfo()
  lines <- c()
  lines <- c(lines, "R version:")
  lines <- c(lines, si$R.version$version.string)
  lines <- c(lines, "")
  lines <- c(lines, "Platform:")
  lines <- c(lines, si$R.version$platform)
  lines <- c(lines, "")
  lines <- c(lines, "Attached base packages:")
  lines <- c(lines, paste("-", si$basePkgs))
  lines <- c(lines, "")
  lines <- c(lines, "Other attached packages:")
  for (pkg in si$otherPkgs) {
    lines <- c(
      lines,
      paste("- ", pkg$Package, " (", pkg$Version, ")", sep = "")
    )
  }
  return(lines)
}

.tidyStackTrace <- function(trace) {
  # saveRDS(trace, sprintf("s:/temp/trace_%d.rds", length(trace)))
  if (getThreadNumber() == 0) {
    if (length(trace) > 4 && grepl("echoToConsole = FALSE", trace[length(trace) - 4])) {
      # Captured via globalCallingHandlers(): 2 more layers to discard
      offset <- 2
    } else {
      offset <- 0
    }
    trace <- trace[1:(length(trace) - 3 - offset)]
  } else {
    if (length(trace) > 5 && grepl("function \\(e\\).*ParallelLogger::log", trace[length(trace) - 5])) {
      # Captured via withCallingHandlers(): 2 more layers to discard
      offset <- 2
    } else {
      offset <- 0
    }
    trace <- trace[23:(length(trace) - 4 - offset)]
  }
  trace <- paste(1:length(trace), trace, sep = ": ")
  trace <- rev(trace)
  return(trace)
}
