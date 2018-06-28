library(shiny)
library(DT)

shinyServer(function(input, output, session) {
  output$logTable <- renderDataTable({
    visibleLevels <- levels[which(levels == input$level):length(levels)]
    idx <- eventLog$Level %in% visibleLevels
    if (input$thread != "All") {
      idx <- idx & eventLog$Thread == input$thread
    }
    if (input$package != "All") {
      idx <- idx & eventLog$Package == input$package
    }
    options = list(pageLength = 10000, 
                   searching = TRUE, 
                   lengthChange = FALSE,
                   ordering = FALSE,
                   paging = FALSE,
                   scrollY = '75vh')
    selection = list(mode = "single", target = "row")
    table <- datatable(eventLog[idx, ], 
                       options = options, 
                       selection = selection,
                       rownames = FALSE,
                       class = "stripe nowrap compact")
    table <- formatStyle(table = table, 
                         columns = 3,
                         target = "row",
                         backgroundColor = styleEqual(colorLevels, colors))
    return(table)
  })
})

