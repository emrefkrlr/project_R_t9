#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("model.R", local = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Copy the line below to make a text input box
  textInput("text", label = h3("Text input")),
  
  hr(),
  fluidRow(column(10, verbatimTextOutput("value")))
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  output$value <- renderText({prediction_result <- word_prediction(input$text)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

