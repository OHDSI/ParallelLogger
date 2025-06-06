# @file ShinyApps.R
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

#' Launch the log viewer Shiny app
#'
#' @param logFileName   Name of the log file to view.
#'
#' @details
#' Launches a Shiny app that allows the user to view a log file created using the default file logger.
#' Use \code{\link{addDefaultFileLogger}} to start the default file logger.
#'
#' @examples
#' # Create a log file:
#' logFile <- file.path(tempdir(), "log.txt")
#' addDefaultFileLogger(logFile)
#' logInfo("Hello world")
#'
#' # Launch the log file viewer (only if in interactive mode):
#' if (interactive()) {
#'   launchLogViewer(logFile)
#' }
#'
#' # Delete the log file:
#' unlink(logFile)
#' @export
launchLogViewer <- function(logFileName) {
  ensure_installed("shiny")
  ensure_installed("DT")
  appDir <- system.file("shinyApps", "LogViewer", package = "ParallelLogger")
  .GlobalEnv$logFileName <- normalizePath(logFileName)
  on.exit(rm(logFileName, envir = .GlobalEnv))
  shiny::runApp(appDir)
}

# Borrowed from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L44
is_installed <- function(pkg, version = "0") {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

# Borrowed and adapted from devtools:
# https://github.com/hadley/devtools/blob/ba7a5a4abd8258c52cb156e7b26bb4bf47a79f0b/R/utils.r#L74
ensure_installed <- function(pkg) {
  if (!is_installed(pkg)) {
    msg <- paste0(sQuote(pkg), " must be installed for this functionality.")
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (menu(c("Yes", "No")) == 1) {
        install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}
