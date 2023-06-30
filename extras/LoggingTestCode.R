# Some random testing code used during development

library(ParallelLogger)


ParallelLogger:::registerDefaultHandlers()

# By default there is one logger with threshold = "INFO" that writes to the console:

logInfo("Hello world")

# We can replace this with a fancier logger, for example with threshold = "DEBUG":

clearLoggers()
addDefaultConsoleLogger()

registerLogger(createLogger(threshold = "INFO",
                            appenders = list(createConsoleAppender(layout = layoutSimple))))

unlink("c:/temp/logFile.txt")
addDefaultFileLogger("c:/temp/logFile.txt")

registerLogger(createLogger(name = "STACKTRACE",
                            threshold = "WARN",
                            appenders = list(createFileAppender(layout = layoutStackTrace,
                                                                fileName = "c:/temp/errorLog.txt"))))

addDefaultFileLogger("c:/temp/errorLog.txt")
addDefaultEmailLogger(mailSettings, test = TRUE)

# Throws an error:
do.call(registerLogger, list())

registerLogger(NULL)
stop("tst")
warning("tst")

unregisterLogger("STACKTRACE")
unregisterLogger("DEFAULT_FILE_LOGGER")
unregisterLogger("DEFAULT_ERRORREPORT_LOGGER")

writeLines(SqlRender::readSql("c:/temp/errorLog.txt"))
unlink("c:/temp/errorLog.txt")

logDebug("Hello world")

library(EmpiricalCalibration)
fitNull(logRr = "sadf", seLogRr = "asdf")

data(sccs)
negatives <- sccs[sccs$groundTruth == 0, ]
negatives$seLogRr <- NA
null <- fitNull(negatives$logRr, negatives$seLogRr)

# We can add a second logger that logs to a file:


# Messages from separate threads will also be logged to the same file:
a <- 3
cluster <- makeCluster(3)
fun <- function(x) {
  ParallelLogger::logFatal("Hello")
  return(ParallelLogger::getLoggers())
}
dummy <- clusterApply(cluster, 1:10, fun)
dummy[[1]]
stopCluster(cluster)



# A convenient way to view the log file is with the log viewer:
launchLogViewer("c:/temp/logFile.txt")


stop("asdf")

options(error = function() str(geterrmessage()))


clearLoggers()
registerLogger(createLogger(threshold = "DEBUG",
                            appenders = list(createConsoleAppender(layout = layoutParallel))))



a <- b

logError("asd\naaa")

options(warning.expression = substitute(print(sys.call(-4)[[2]])))

warning("hi my name is mud")


myfunc <- function () {
  for (i in 1:sys.nframe()) {  
    if (sys.call(-i)[[1]] == ".signalSimpleWarning") {
      print(sys.call(-i)[[2]])
      break
    }
  }
}
options (warning.expression=quote (myfunc ()))

x <- 1
warning ("test", x)

stop("Test")

# Logging in threads -----------------------------------
library(ParallelLogger)
fileName <- "s:/temp/logFile.txt"
addDefaultFileLogger(fileName)
addDefaultErrorReportLogger(fileName)
logFatal("Oops")
stop("Yikes")
message("start")

cluster <- makeCluster(3)
fun <- function(x) {
  ParallelLogger::logInfo(paste("Hello", x))
  message(paste("Hi", x))
  ParallelLogger::logFatal("There")
  # if (x == 10) 
    # stop("Halt!")
  return()
}
dummy <- clusterApply(cluster, 1:10, fun)
stopCluster(cluster)


log <- readChar(fileName, file.info(fileName)$size)
writeLines(log)


unlink(fileName)
unregisterLogger("DEFAULT_FILE_LOGGER")
# unregisterLogger("DEFAULT_ERRORREPORT_LOGGER")

sysStatus <- readRDS("S:/temp/sysStatus.rds")


data <- EmpiricalCalibration::simulateControls()
data$seLogRr <- NA
EmpiricalCalibration::plotCalibrationEffect(data$logRr, data$seLogRr, fileName = "s:/temp/plot.png")





trace <- readRDS("s:/temp/trace_29.rds")
