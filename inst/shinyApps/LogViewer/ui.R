library(shiny)
library(DT)

shinyUI(
  fluidPage(
    titlePanel(paste("Log File Viewer -", logFileName)),
    fluidRow(
      column(2,
             actionButton("firstButton", label = "|<", title = "First chunk"),
             actionButton("backButton", label = "<", title = "Previous chunk"),
             actionButton("forwardButton", label = ">", title = "Next chunk"),
             actionButton("lastButton", label = ">|", title = "Last chunk"),
             uiOutput("progressUi"),
             selectInput("level", label = "Level", choices = levels, selected = "INFO"),
             selectInput("thread", label = "Thread", choices = c()),
             selectInput("package", label = "Package", choices = c())
             ),
      column(10,
             dataTableOutput("logTable"),
             uiOutput("detailsUi"))
    )
  )
)
