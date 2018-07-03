
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  verticalLayout(
    titlePanel("Control Lot Evaluation"),
    mainPanel(
      
      fluidRow( 
      column(verbatimTextOutput("expt"),width=6),
      
      column(verbatimTextOutput("message",placeholder = TRUE),width=6)
      ),
    
    
      fluidRow(
        column(verticalLayout(
                  tableOutput("exptDetail"),
                  tags$hr(),
                  tableOutput("exptResults")
        )
                  ,width = 8),
             
        column(plotOutput("plot"),width=4)
        ),
        

      
    
    actionButton("report","Create Report"),
      
   uiOutput("return")
    )
  )
)

