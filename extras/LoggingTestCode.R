# This may become a vignette later on

library(ParallelLogger)

# By default there is one logger with threshold = "INFO" that writes to the console:

logInfo("Hello world")

# We can replace this with a fancier logger, for example with threshold = "DEBUG":

clearLoggers()

registerLogger(createLogger(threshold = "INFO",
                            appenders = list(createConsoleAppender(layout = layoutSimple))))

unlink("c:/temp/logFile.txt")
addDefaultFileLogger("c:/temp/logFile.txt")

logDebug("Hello world")

# We can add a second logger that logs to a file:


# Messages from separate threads will also be logged to the same file:
a <- 3
cluster <- makeCluster(3)
fun <- function(x) {
  warning("warn: Value of runif is ", runif(1))
  if (x == 6 || x == 9)
    x <- a
  return(NULL)
}
dummy <- clusterApply(cluster, 1:10, fun)

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

