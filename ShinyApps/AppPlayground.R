library(shiny)
library(shinythemes)
library(ggplot2)
library(readxl)
library(xlsx)


ui <- fluidPage(
  theme = shinytheme("sandstone"),
  "Livios first R App",
  
  textInput("Name", "Name:", ""),
  textInput("Email", "Email: ", ""),
  selectInput("Selection", "Chose an option", choices = c("Java", "R", "Python")),
  selectInput("download", "Select Data to download", choices = c("euro", "mtcars", "iris")),
  
  hr(),
  h4("Here is your information"),
  verbatimTextOutput("txtout"),
  #sliderInput("sliderinput", "Select a value", min=10, max=20, value = 12)
  )
  
server <- function(input, output){
  output$txtout <- renderText((
    paste(input$Name, input$Email, input$Selection)
  ))
}

shinyApp(ui = ui, server = server)
