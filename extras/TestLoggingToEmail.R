# Test mail notifications 

mailSettings <- list(
  from = keyring::key_get("mailAddress"),
  to = keyring::key_get("mailToAddress"),
  engine = "curl",
  engineopts = list(username = keyring::key_get("mailUser"),
                    password = keyring::key_get("mailPassword")),
  control = list(smtpServer = paste(keyring::key_get("mailSmtpServer"),
                                    keyring::key_get("mailSmtpPort"),
                                    sep = ":"))
)

# args <- mailSettings
# args$subject <- "SMTP auth test"
# args$msg <- sendmailR::mime_part("This message was send using sendmailR and curl.")
# do.call(sendmailR::sendmail, args)

library(ParallelLogger)

addDefaultEmailLogger(mailSettings)
stop("Logging test")

unregisterLogger("DEFAULT_EMAIL_LOGGER")



# Verify an error is thrown if user uses mailR settings ------------------------
mailSettings <- list(from = Sys.getenv("mailAddress"),
                     to = c(Sys.getenv("mailToAddress")),
                     smtp = list(host.name = keyring::key_get("mailSmtpServer"),
                                 port = keyring::key_get("mailSmtpPort"),
                                 user.name = Sys.getenv("mailAddress"),
                                 passwd = Sys.getenv("mailPassword"),
                                 ssl = TRUE),
                     authenticate = TRUE,
                     send = TRUE)
addDefaultEmailLogger(mailSettings)
