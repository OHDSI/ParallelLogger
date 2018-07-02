

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

runAndNotify({
  a <- b
}, mailSettings = mailSettings, label = "Fancy code")




createArgFunction(functionName = "runAndNotify", rCode = "")
