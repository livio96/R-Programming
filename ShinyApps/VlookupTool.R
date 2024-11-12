# Install and load necessary packages
# Install packages if not already installed
# install.packages(c("shiny", "readr", "readxl", "writexl", "dplyr"))

library(shiny)
library(readr)
library(readxl)
library(writexl)
library(dplyr)

ui <- fluidPage(
  titlePanel("VLOOKUP Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("primaryFile", "Upload Primary File (CSV or Excel)"),
      fileInput("secondaryFile", "Upload Secondary File (CSV or Excel)"),
      uiOutput("primaryKeySelectUI"),
      uiOutput("secondaryKeySelectUI"),
      uiOutput("columnsSelectUI"),
      actionButton("submit", "Submit"),
      radioButtons("downloadFormat", "Select Download Format:", choices = c("CSV", "Excel")),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  
  primaryData <- reactive({
    req(input$primaryFile)
    ext <- tools::file_ext(input$primaryFile$name)
    if (ext == "csv") {
      read_csv(input$primaryFile$datapath)
    } else if (ext %in% c("xls", "xlsx")) {
      read_excel(input$primaryFile$datapath)
    } else {
      showNotification("Unsupported file format for primary file.", type = "error")
      return(NULL)
    }
  })
  
  secondaryData <- reactive({
    req(input$secondaryFile)
    ext <- tools::file_ext(input$secondaryFile$name)
    if (ext == "csv") {
      read_csv(input$secondaryFile$datapath)
    } else if (ext %in% c("xls", "xlsx")) {
      read_excel(input$secondaryFile$datapath)
    } else {
      showNotification("Unsupported file format for secondary file.", type = "error")
      return(NULL)
    }
  })
  
  output$primaryKeySelectUI <- renderUI({
    req(primaryData())
    selectInput("primaryKeyColumn", "Select VLOOKUP Column from Primary File:", choices = names(primaryData()))
  })
  
  output$secondaryKeySelectUI <- renderUI({
    req(secondaryData())
    selectInput("secondaryKeyColumn", "Select VLOOKUP Column from Secondary File:", choices = names(secondaryData()))
  })
  
  output$columnsSelectUI <- renderUI({
    req(secondaryData())
    cols <- names(secondaryData())
    checkboxGroupInput("columnsToAdd", "Select Columns to Bring Over from Secondary File:", choices = cols)
  })
  
  mergedData <- eventReactive(input$submit, {
    req(primaryData(), secondaryData(), input$primaryKeyColumn, input$secondaryKeyColumn, input$columnsToAdd)
    
    # Prepare secondary data with selected columns
    secondarySelected <- secondaryData() %>%
      select(all_of(c(input$secondaryKeyColumn, input$columnsToAdd))) %>%
      distinct()
    
    # Perform left join
    merged <- left_join(primaryData(), secondarySelected,
                        by = setNames(input$secondaryKeyColumn, input$primaryKeyColumn))
    
    merged
  })
  
  output$preview <- renderTable({
    req(mergedData())
    head(mergedData())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$downloadFormat == "Excel") {
        "merged_data.xlsx"
      } else {
        "merged_data.csv"
      }
    },
    content = function(file) {
      data <- mergedData()
      if (input$downloadFormat == "Excel") {
        write_xlsx(data, path = file)
      } else {
        write_csv(data, file)
      }
    }
  )
}

shinyApp(ui, server)
