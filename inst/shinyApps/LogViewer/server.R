library(shiny)
library(DT)

truncScript <- "function(data, type, row, meta) {\n
      return type === 'display' && data != null && data.length > 100 ?\n
        '<span title=\"' + data.substr(0, 1000) + '\">' + data.substr(0, 100) + '...</span>' : data;\n
     }"

fileStatus <- new.env()
fileStatus$fileSize <- file.info(logFileName)$size
fileStatus$eof <- FALSE
fileStatus$start <- 0
fileStatus$firstLine <- ""
fileStatus$atStartOfLine <- TRUE

parseLines <- function(lines) {
  rows <- strsplit(lines, "\t")
  malformed <- sapply(rows, function(x) length(x) != 6)
  rows <- rows[!malformed]
  result <- data.frame(Timestamp = as.POSIXct(sapply(rows, function(x) x[1])),
                       Thread = sapply(rows, function(x) x[2]),
                       Level = sapply(rows, function(x) x[3]),
                       Package = sapply(rows, function(x) x[4]),
                       Function = sapply(rows, function(x) x[5]),
                       Message = sapply(rows, function(x) gsub("\"", "&quot;", x[6])))
  return(result)
}

readNextRows <- function(n = batchSize) {
  lines <- readLines(fileConnection, n)
  fileStatus$firstLine <- lines[1]
  fileStatus$eof <- length(lines) < n
  return(parseLines(lines))
}

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(eventLog = readNextRows(batchSize))
  
  selectedRow <- reactive({
    idx <- input$logTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      row <- values$eventLog[idx, ]
      return(row)
    }
  })
  
  observe({
    threads <- as.character(unique(values$eventLog$Thread))
    threads <- threads[order(threads)]
    threads <- c("All", threads)
    packages <- as.character(unique(values$eventLog$Package))
    packages <- c("All", packages)
    updateSelectInput(inputId = "thread", choices = threads)
    updateSelectInput(inputId = "package", choices = packages)
  })
  
  output$logTable <- renderDataTable({
    
    visibleLevels <- levels[which(levels == input$level):length(levels)]
    idx <- values$eventLog$Level %in% visibleLevels
    if (input$thread != "All") {
      idx <- idx & values$eventLog$Thread == input$thread
    }
    if (input$package != "All") {
      idx <- idx & values$eventLog$Package == input$package
    }
    options = list(searching = TRUE, 
                   lengthChange = FALSE,
                   ordering = FALSE,
                   paging = FALSE,
                   info = FALSE,
                   scrollY = '60vh',
                   columnDefs = list(list(
                     targets = c(5),
                     render = JS(truncScript)
                   )))
    selection = list(mode = "single", target = "row")
    table <- datatable(values$eventLog[idx, ], 
                       options = options, 
                       selection = selection,
                       rownames = FALSE,
                       escape = FALSE,
                       class = "stripe nowrap compact")
    table <- formatStyle(table = table, 
                         columns = 3,
                         target = "row",
                         backgroundColor = styleEqual(colorLevels, colors))
    return(table)
  })
  
  output$detailsUi <- renderUI({
    row <- selectedRow()
    if (is.null(row)) {
      return(HTML("<p>Select a row to see details</p>"))
    } else {
      lines <- list("<table>",
                    sprintf("<tr><td><strong>Timestamp</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Timestamp),
                    sprintf("<tr><td><strong>Thread</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Thread),
                    sprintf("<tr><td><strong>Level</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Level),
                    sprintf("<tr><td><strong>Package</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Package),
                    sprintf("<tr><td><strong>Function</strong></td><td>&nbsp;&nbsp;</td><td>%s</td></tr>", row$Function),
                    "</table>",
                    "<h2>Message</h2>",
                    sprintf("<p>%s</p>", row$Message))
      return(HTML(paste(lines, collapse = "\n")))
      
    }
  })
  
  observeEvent(input$forwardButton, {
    if (fileStatus$eof) {
      return()
    }
    if (!fileStatus$atStartOfLine) {
      partialLine <- readLines(fileConnection, 1)
    }
    fileStatus$start <- seek(fileConnection, NA)
    fileStatus$atStartOfLine <- TRUE
    values$eventLog <- readNextRows(batchSize)
  })
  
  observeEvent(input$firstButton, {
    if (fileStatus$start == 0) {
      return()
    }
    fileStatus$start <- 0
    seek(fileConnection, 0)
    fileStatus$atStartOfLine <- TRUE
    values$eventLog <- readNextRows(batchSize)
  })
  
  observeEvent(input$backButton, {
    if (fileStatus$start == 0) {
      return()
    }
    start <- max(0, fileStatus$start - (batchSize*10000))
    seek(fileConnection, start)
    if (start == 0) {
      partialLine <- ""
    } else {
      # Jump to start of a new line:
      partialLine <- readLines(fileConnection, 1)
    }
    endIdx <- c()
    lines <- c()
    while (length(endIdx) == 0) {
      newLines <- readLines(fileConnection, batchSize)
      lines <- c(lines, newLines)
      endIdx <- which(lines == fileStatus$firstLine) - 1
      if (length(newLines) < batchSize && length(endIdx) == 0) {
        endIdx <- length(lines)
        print("Could not find line")
      }
    }
    endIdx <- endIdx[1]
    startIdx <- max(1, endIdx - batchSize + 1)
    # Guess new start position in file:
    start <- start + nchar(partialLine)
    if (startIdx > 1) {
      start <- start + sum(nchar(lines[1:(startIdx - 1)])) + nchar(partialLine)
    } 
    fileStatus$start <- start
    fileStatus$firstLine <- lines[startIdx]
    fileStatus$eof <- FALSE
    fileStatus$atStartOfLine <- FALSE
    values$eventLog <- parseLines(lines[startIdx:endIdx])
    
    # Set file cursor to (roughly) end of last line:
    seek(fileConnection, start + sum(nchar(lines[startIdx:endIdx])))
  })
  
  observeEvent(input$lastButton, {
    if (fileStatus$eof) {
      return()
    }
    start <- max(0, fileStatus$fileSize - (batchSize*10000))
    seek(fileConnection, start)
    if (start == 0) {
      partialLine <- ""
    } else {
      # Jump to start of a new line:
      partialLine <- readLines(fileConnection, 1)
    }
    lines <- readLines(fileConnection, -1)
    endIdx <- length(lines)
    startIdx <- max(1, endIdx - batchSize + 1)
    # Guess new start position in file:
    start <- start + nchar(partialLine)
    if (startIdx > 1) {
      start <- start + sum(nchar(lines[1:(startIdx - 1)])) + nchar(partialLine)
    } 
    fileStatus$start <- start
    fileStatus$firstLine <- lines[startIdx]
    fileStatus$eof <- TRUE
    fileStatus$atStartOfLine <- FALSE
    values$eventLog <- parseLines(lines[startIdx:endIdx])
    
  })
  
  output$progressUi <- renderUI({
    # Dummy call to trigger refresh when new rows are read:
    nrow(values$eventLog)
    start <- as.integer(100 * fileStatus$start / fileStatus$fileSize)
    if (fileStatus$eof) {
      end <- 100
    } else {
      end <- as.integer(100 * min(seek(fileConnection, NA) / fileStatus$fileSize, 1))
    }
    width <- as.integer(end - start)
    if (width == 0) {
      width <- 1
    }
    html <- c("<div style=\"position:relative; margin-top:5px; margin-bottom:5px; background-color:#DFF1FF; width:100%; height:20px; padding:0px;\">",
              sprintf("<div style=\"width:%d%%; left:%d%%; position:absolute; background-color:#0063B0;\">", width, start),
              "&nbsp;</div></div>")
    return(HTML(html))
  })
  
})

