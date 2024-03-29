% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyApps.R
\name{launchLogViewer}
\alias{launchLogViewer}
\title{Launch the log viewer Shiny app}
\usage{
launchLogViewer(logFileName)
}
\arguments{
\item{logFileName}{Name of the log file to view.}
}
\description{
Launch the log viewer Shiny app
}
\details{
Launches a Shiny app that allows the user to view a log file created using the default file logger.
Use \code{\link{addDefaultFileLogger}} to start the default file logger.
}
\examples{
# Create a log file:
logFile <- file.path(tempdir(), "log.txt")
addDefaultFileLogger(logFile)
logInfo("Hello world")

# Launch the log file viewer (only if in interactive mode):
if (interactive()) {
  launchLogViewer(logFile)
}

# Delete the log file:
unlink(logFile)
}
