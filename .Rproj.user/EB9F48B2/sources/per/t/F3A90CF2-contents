#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) { 
  
  options(shiny.maxRequestSize=200*1024^2)
  
  datos <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    tryCatch ({
      ifelse(input$columname, res <- read.csv(inFile$datapath, header=input$header, sep=input$sep, dec = input$dec, row.names = 1), 
             res <- read.csv(inFile$datapath, header=input$header, sep=input$sep, dec = input$dec))
      res <- na.omit(res)
      return(res)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  options(DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE))
  output$contents = DT::renderDT(datos(), selection = 'none', server = FALSE, editable = TRUE, filter = 'bottom')
})
