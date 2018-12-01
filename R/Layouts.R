# @file Layouts.R
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
