# Load necessary libraries
library(shiny)
library(dplyr)
library(DT)
library(scales)  # For currency formatting
library(writexl)  # For exporting to Excel

# Increase file upload size limit to 100 MB
options(shiny.maxRequestSize = 100*1024^2)

# Define UI for the app
ui <- fluidPage(
  
  # Application title
  titlePanel("Purchase Order Profitability Dashboard"),
  
  # Upload and download options at the top in a compact layout
  fluidRow(
    column(6,
           fileInput("receivedFile", "Upload Received CSV", accept = ".csv"),
           fileInput("inventoryFile", "Upload Inventory CSV", accept = ".csv"),
           fileInput("fulfilledFile", "Upload Fulfilled CSV", accept = ".csv")
    ),
    column(6, 
           downloadButton("exportAgentExcel", "Export Agent Excel"),
           downloadButton("exportDocExcel", "Export PO Excel")
    )
  ),
  
  # Main panel for displaying outputs
  fluidRow(
    column(12, 
           h3("Sales Agent Summary"),
           DTOutput("agentTable")
    ),
    column(12,
           h3("Purchase Order Breakdown"),
           DTOutput("docTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load the received dataset
  received <- reactive({
    req(input$receivedFile)
    read.csv(input$receivedFile$datapath)
  })
  
  # Load the inventory dataset
  inventory <- reactive({
    req(input$inventoryFile)
    read.csv(input$inventoryFile$datapath)
  })
  
  # Load the fulfilled dataset
  fulfilled <- reactive({
    req(input$fulfilledFile)
    read.csv(input$fulfilledFile$datapath)
  })
  
  # Process the summary metrics grouped by Sales Rep (Sales Agent)
  agentSummary <- reactive({
    received_data <- received()
    inventory_data <- inventory()
    fulfilled_data <- fulfilled()
    
    received_data %>%
      group_by(Sales.Rep) %>%
      summarise(
        Quantity.Purchased = n(),
        Remaining.Quantity = sum(Serial.Number %in% inventory_data$Inventory.Number),
        Total.Purchase.Rate = sum(Item.Rate),
        Remaining.Value = sum(Item.Rate * (Serial.Number %in% inventory_data$Inventory.Number)),
        Total.Sales.Amount = sum(fulfilled_data$Rate[fulfilled_data$Serial.Number %in% Serial.Number]),
        Total.Fees = sum(fulfilled_data$Fees[fulfilled_data$Serial.Number %in% Serial.Number]),
        Total.Gross.Profit = sum(fulfilled_data$Rate[fulfilled_data$Serial.Number %in% Serial.Number]) - sum(Item.Rate[Serial.Number %in% fulfilled_data$Serial.Number]),
        Net.Profit = Total.Gross.Profit - Total.Fees
      ) %>%
      mutate(
        Total.Purchase.Rate = dollar(Total.Purchase.Rate),
        Remaining.Value = dollar(Remaining.Value),
        Total.Sales.Amount = dollar(Total.Sales.Amount),
        Total.Fees = dollar(Total.Fees),
        Total.Gross.Profit = dollar(Total.Gross.Profit),
        Net.Profit = dollar(Net.Profit)
      )
  })
  
  # Render the Sales Agent summary table
  output$agentTable <- renderDT({
    datatable(agentSummary(), selection = 'single')
  })
  
  # Render the Document Number breakdown based on the selected Sales Agent
  output$docTable <- renderDT({
    req(input$agentTable_rows_selected)  # Ensure a row is selected
    
    selected_agent <- agentSummary()$Sales.Rep[input$agentTable_rows_selected]
    
    received_data <- received()
    inventory_data <- inventory()
    fulfilled_data <- fulfilled()
    
    # Filter data by the selected sales agent
    received_data %>%
      filter(Sales.Rep == selected_agent) %>%
      group_by(Document.Number) %>%
      summarise(
        Quantity.Purchased = n(),
        Remaining.Quantity = sum(Serial.Number %in% inventory_data$Inventory.Number),
        Total.Purchase.Rate = sum(Item.Rate),
        Remaining.Value = sum(Item.Rate * (Serial.Number %in% inventory_data$Inventory.Number)),
        Total.Sales.Amount = sum(fulfilled_data$Rate[fulfilled_data$Serial.Number %in% Serial.Number]),
        Total.Fees = sum(fulfilled_data$Fees[fulfilled_data$Serial.Number %in% Serial.Number]),
        Total.Gross.Profit = sum(fulfilled_data$Rate[fulfilled_data$Serial.Number %in% Serial.Number]) - sum(Item.Rate[Serial.Number %in% fulfilled_data$Serial.Number]),
        Net.Profit = Total.Gross.Profit - Total.Fees
      ) %>%
      mutate(
        Total.Purchase.Rate = dollar(Total.Purchase.Rate),
        Remaining.Value = dollar(Remaining.Value),
        Total.Sales.Amount = dollar(Total.Sales.Amount),
        Total.Fees = dollar(Total.Fees),
        Total.Gross.Profit = dollar(Total.Gross.Profit),
        Net.Profit = dollar(Net.Profit)
      ) %>%
      datatable()
  })
  
  # Export grouped by agent as Excel
  output$exportAgentExcel <- downloadHandler(
    filename = function() {
      paste("Agent_Summary_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(agentSummary(), file)
    }
  )
  
  # Export grouped by document number (PO) as Excel
  output$exportDocExcel <- downloadHandler(
    filename = function() {
      paste("PO_Summary_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(input$agentTable_rows_selected)
      selected_agent <- agentSummary()$Sales.Rep[input$agentTable_rows_selected]
      
      received_data <- received()
      inventory_data <- inventory()
      fulfilled_data <- fulfilled()
      
      doc_summary <- received_data %>%
        filter(Sales.Rep == selected_agent) %>%
        group_by(Document.Number) %>%
        summarise(
          Quantity.Purchased = n(),
          Remaining.Quantity = sum(Serial.Number %in% inventory_data$Inventory.Number),
          Total.Purchase.Rate = sum(Item.Rate),
          Remaining.Value = sum(Item.Rate * (Serial.Number %in% inventory_data$Inventory.Number)),
          Total.Sales.Amount = sum(fulfilled_data$Rate[fulfilled_data$Serial.Number %in% Serial.Number]),
          Total.Fees = sum(fulfilled_data$Fees[fulfilled_data$Serial.Number %in% Serial.Number]),
          Total.Gross.Profit = sum(fulfilled_data$Rate[fulfilled_data$Serial.Number %in% Serial.Number]) - sum(Item.Rate[Serial.Number %in% fulfilled_data$Serial.Number]),
          Net.Profit = Total.Gross.Profit - Total.Fees
        )
      
      write_xlsx(doc_summary, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
