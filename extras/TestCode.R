
insertCohortDefinitionInPackage(definitionId = 5021, 
                                name = "Test", 
                                baseUrl = Sys.getenv("baseUrl"))



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



# WebAPI functions -----------------------------------------------------------

getCohortDefinitionName(baseUrl = Sys.getenv("baseUrl"), definitionId = 5021)

getConceptSetName(baseUrl = Sys.getenv("baseUrl"), setId = 12)

getPriorityVocabKey(baseUrl = Sys.getenv("baseUrl"))

getConceptSetConceptIds(baseUrl = Sys.getenv("baseUrl"), setId = 12)


# Args functions --------------------------------------------------------------


createArgFunction(functionName = "getCohortDefinitionName", rCode = "")
