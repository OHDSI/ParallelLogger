# Test mail notifications -------------------------------------------------
mailSettings <- list(from = Sys.getenv("mailAddress"),
                     to = c(Sys.getenv("mailAddress")),
                     smtp = list(host.name = "smtp.gmail.com",
                                 port = 465,
                                 user.name = Sys.getenv("mailAddress"),
                                 passwd = Sys.getenv("mailPassword"),
                                 ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)


# mailR::send.mail(from = mailSettings$from,
#                  to = mailSettings$to,
#                  subject = "My test",
#                  body = "This is a test",
#                  smtp = mailSettings$smtp,
#                  authenticate = mailSettings$authenticate,
#                  send = mailSettings$send)
                 
library(ParallelLogger)

addDefaultEmailLogger(mailSettings)
stop("Logging test")
unregisterLogger("DEFAULT")

logger <- createLogger(name = "EMAIL",
                       threshold = "INFO",
                       appenders = list(createEmailAppender(mailSettings = mailSettings, test = TRUE)))


registerLogger(logger)
logInfo("Hello world")
logFatal("Hello world")
stop("Bad thing")

unregisterLogger(logger)

f <- function(x) {
  print(a) 
}

f(1)
addDefaultFileLogger("s:/temp/log.txt")
x <- getLoggers()
unregisterLogger("DEFAULT")

logger <- createLogger(name = "EMAIL",
                       threshold = "INFO",
                       appenders = list(createEmailAppender(mailSettings = mailSettings, 
                                                            layout = layoutEmail,
                                                            test = TRUE)))

# Test arg function code -------------------------------------------
writeLines(createArgFunction("createArgFunction"))




fun <- function(x, dummy, foo) {
  if (x == 3)
    stop("asdf'")
  return(x^2)
}

cluster <- makeCluster(numberOfThreads = 3)
x <- clusterApply(cluster, 1:3, fun, dummy = "hello", foo = cars)
stopCluster(cluster)
x
