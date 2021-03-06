% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Layouts.R
\name{layoutErrorReport}
\alias{layoutErrorReport}
\title{Logging layout for error report}
\usage{
layoutErrorReport(level, message)
}
\arguments{
\item{level}{The level of the message (e.g. "INFO")}

\item{message}{The message to layout.}
}
\description{
A layout function to be used with an appender. This layout creates a more elaborate error message,
for sharing with the developer. If an error occurs in the main thread a summary of the system info
will be included.
}
