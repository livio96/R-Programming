library(shiny)
library(shinythemes)
library(ggplot2)
library(readxl)
library(xlsx)
library(dplyr)
library(tidyverse)
library(raster)


################________Developed By Livio Beqiri - On 11/27/2023 _______###############


##########_____Simple UI_____############

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  h4("Inventory Dashboard"),
  #textInput("Email", "Email: ", ""),
  selectInput(
    "Condition",
    "Select Condition",
    choices = c(
      "All",
      "New",
      "New Open Box",
      "Like New",
      "Recertified",
      "Refresh",
      "Excess"
    )
  ),
  selectInput(
    "Manufacturer",
    "Manufacturer",
    choices = c("All", "Cisco", "Plantronics")
  ),
  #selectInput("download", "Select Data to download", choices = c("euro", "mtcars", "iris")),
  #textInput("Manufacturer", "Manufacturer: ", ""),
  
  
  br(),
  hr(),
  verbatimTextOutput("txtout"),
  
  fluidRow(column(12,
                  DT::dataTableOutput('table')))
)


######________Functionality______##################

server <- function(input, output) {
  file_path <-
    '/Users/liviobeqiri/Documents/R/R-Programming/InventoryReport.xlsx'
  raw_data <- read_excel(file_path)
  df <- as.data.frame(raw_data)
  
  observe({
    if (input$Condition == "New") {
      df_new <- filter(df, CONDITION == "New")
      output$table <- DT::renderDataTable(df_new)
    }
    else if (input$Condition == "Like New") {
      df_like_new <- filter(df, CONDITION == 'Like New')
      output$table <- DT::renderDataTable(df_like_new)
    }
    else if (input$Condition == "Recertified") {
      df_recertified <- filter(df, CONDITION == 'Recertified')
      output$table <- DT::renderDataTable(df_recertified)
    }
    else if (input$Condition == "Refresh") {
      df_refresh <- filter(df, CONDITION == 'Refresh')
      output$table <- DT::renderDataTable(df_refresh)
    }
    else if (input$Condition == "New Open Box") {
      df_new_open_box <- filter(df, CONDITION == 'New Open Box')
      output$table <- DT::renderDataTable(df_new_open_box)
    }
    else if (input$Condition == "Excess") {
      df_excess <- filter(df, CONDITION == 'Excess')
      output$table <- DT::renderDataTable(df_excess)
    }
    else {
      output$table <- DT::renderDataTable(df)
    }
    
  })
  
  observe({
    if (input$Manufacturer == "Cisco") {
      cisco_df <- filter(df, MANUFACTURER == "Cisco")
      output$table <- DT::renderDataTable(cisco_df)
    }
    else {
      output$table <- DT::renderDataTable(df)
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)
