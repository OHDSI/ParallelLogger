library(shiny)
library(DT)

shinyUI(
  fluidPage(
    titlePanel(paste("Log File Viewer -", logFileName)),
    fluidRow(
      column(1,
             selectInput("level", label = "Level", choices = levels, selected = "INFO"),
             selectInput("thread", label = "Thread", choices = threads),
             selectInput("package", label = "Package", choices = packages)),
      column(11,
             dataTableOutput("logTable"))
    )
  )
)
