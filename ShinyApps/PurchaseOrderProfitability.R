library(shiny)
library(dplyr)
library(DT)
library(scales)
library(writexl)

options(shiny.maxRequestSize = 100 * 1024^2)

urls <- list(
  received = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20.csv",
  inventory = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20.csv",
  fulfilled = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20.csv",
  kits = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20Fullfilled%20Kits.csv"
)

ui <- fluidPage(
  titlePanel("Purchase Order Profitability Dashboard"),
  uiOutput("appUI")
)

server <- function(input, output, session) {
  # Track authentication and loaded data
  authenticated <- reactiveVal(FALSE)
  dataList <- reactiveVal(NULL)
  loadInfo <- reactiveVal(NULL)
  
  # Authentication modal on start
  showModal(modalDialog(
    title = "Authentication Required",
    passwordInput("password", "Password:"),
    footer = tagList(
      actionButton("login", "Login")
    ),
    easyClose = FALSE,
    fade = TRUE
  ))
  
  observeEvent(input$login, {
    if (identical(input$password, "telquestprofit")) {
      authenticated(TRUE)
      removeModal()
      # Load remote CSVs with progress
      withProgress(message = "Loading Netsuite CSVs...", value = 0, {
        step <- 1/4
        incProgress(step, detail = "Received")
        rec <- read.csv(url(urls$received), stringsAsFactors = FALSE)
        incProgress(step, detail = "Inventory")
        inv <- read.csv(url(urls$inventory), stringsAsFactors = FALSE)
        incProgress(step, detail = "Fulfilled")
        ful <- read.csv(url(urls$fulfilled), stringsAsFactors = FALSE)
        incProgress(step, detail = "Kits")
        kits <- read.csv(url(urls$kits), stringsAsFactors = FALSE)
      })
      # Store load info and data
      info <- data.frame(
        Dataset = c("Received", "Inventory", "Fulfilled", "Kits"),
        Records = c(nrow(rec), nrow(inv), nrow(ful), nrow(kits)),
        stringsAsFactors = FALSE
      )
      loadInfo(info)
      dataList(list(received = rec, inventory = inv, fulfilled = ful, kits = kits))
    } else {
      showModal(modalDialog(
        title = "Authentication Failed",
        "Incorrect password. Please try again.",
        passwordInput("password", "Password:"),
        footer = actionButton("login", "Login"),
        easyClose = FALSE
      ))
    }
  })
  
  # Render main UI after authentication
  output$appUI <- renderUI({
    req(authenticated())
    fluidPage(
      fluidRow(
        column(12,
               br(),
               tableOutput("loadInfo"),
               br(),
               downloadButton("exportAgentExcel", "Export Agent Excel"),
               downloadButton("exportDocExcel",   "Export PO Excel")
        )
      ),
      fluidRow(
        column(12, h3("Sales Agent Summary"),      DTOutput("agentTable")),
        column(12, h3("Purchase Order Breakdown"), DTOutput("docTable"))
      )
    )
  })
  
  # Display load info table
  output$loadInfo <- renderTable({ loadInfo() })
  
  # Reactive access to loaded data
  received <- reactive({ req(dataList()); dataList()$received })
  inventory <- reactive({ req(dataList()); dataList()$inventory })
  fulfilled <- reactive({ req(dataList()); dataList()$fulfilled })
  kits     <- reactive({ req(dataList()); dataList()$kits })
  allFulfilled <- reactive(bind_rows(fulfilled(), kits()))
  
  # Identify serial columns in inventory
  invSerials <- reactive({
    inv <- inventory()
    cols <- names(inv)
    serialCol <- case_when(
      'Formula (Text)' %in% cols                        ~ 'Formula (Text)',
      any(grepl('formula.*text', tolower(cols)))        ~ cols[grepl('formula.*text', tolower(cols))][1],
      any(grepl('inventory', tolower(cols)))            ~ cols[grepl('inventory', tolower(cols))][1],
      'Item Inventory Number' %in% cols                 ~ 'Item Inventory Number',
      'Inventory Number' %in% cols                      ~ 'Inventory Number',
      TRUE                                              ~ NA_character_
    )
    req(!is.na(serialCol), "Serial number column not found in Inventory data.")
    unique(trimws(inv[[serialCol]]))
  })
  
  # Merge received and fulfilled data
  mergedData <- reactive({
    rec <- received(); ful <- allFulfilled()
    recSN <- names(rec)[grepl('serial', tolower(names(rec)))][1]
    fulSN <- names(ful)[grepl('serial', tolower(names(ful)))][1]
    req(!is.na(recSN), "Serial number column not found in Received data.")
    req(!is.na(fulSN), "Serial number column not found in Fulfilled data.")
    rateCol <- names(rec)[grepl('rate', tolower(names(rec)))][1]
    req(!is.na(rateCol), "Rate column not found in Received data.")
    rec$Serial.Number <- rec[[recSN]]; rec$Item.Rate <- rec[[rateCol]]
    ful$Serial.Number <- ful[[fulSN]]
    right_join(select(rec, Serial.Number, Item.Rate), ful, by = 'Serial.Number')
  })
  
  # Calculate fees based on channel
  calculateFees <- function(df) {
    chanCol <- names(df)[grepl('channel|etail', tolower(names(df)))][1]
    priceCol <- names(df)[grepl('^rate$|^price$', tolower(names(df)))][1]
    if (!is.null(chanCol)) df$eTail.Channel <- df[[chanCol]]
    if (!is.null(priceCol)) df$Rate <- df[[priceCol]]
    df %>% mutate(
      Fees = case_when(
        eTail.Channel %in% c('Amazon','Newegg','Newegg Business','Walmart','eBay') ~ 0.14 * Rate,
        is.na(eTail.Channel) | eTail.Channel == ''                                ~ 0,
        eTail.Channel == 'Sales Team' & Item.Rate >= Rate                         ~ 0,
        eTail.Channel == 'Sales Team' & Rate > Item.Rate                          ~ 0.14 * (Rate - Item.Rate),
        TRUE                                                                      ~ 0
      )
    )
  }
  
  # Compute PO-level data
  calculatePOData <- reactive({
    rec <- received()
    merged <- calculateFees(mergedData())
    invSN <- invSerials()
    salesCol <- names(rec)[grepl('sales.*rep|rep|agent', tolower(names(rec)))][1]
    docCol   <- names(rec)[grepl('doc|purchase.*order|po', tolower(names(rec)))][1]
    req(!is.na(salesCol), "Sales Rep column not found in Received data.")
    req(!is.na(docCol),   "Document Number column not found in Received data.")
    rec$Sales.Rep <- rec[[salesCol]]; rec$Document.Number <- rec[[docCol]]
    
    rec %>% group_by(Sales.Rep, Document.Number) %>% summarise(
      Quantity.Purchased     = n(),
      Total.Purchase.Rate    = sum(Item.Rate, na.rm = TRUE),
      Cost.of.Sold.Inventory = sum(Item.Rate[Serial.Number %in% merged$Serial.Number], na.rm = TRUE),
      Remaining.Quantity     = sum(Serial.Number %in% invSN),
      Remaining.Value        = sum(Item.Rate[Serial.Number %in% invSN], na.rm = TRUE),
      Total.Sales.Amount     = sum(merged$Rate[merged$Serial.Number %in% Serial.Number], na.rm = TRUE),
      Total.Fees             = sum(merged$Fees[merged$Serial.Number %in% Serial.Number], na.rm = TRUE),
      .groups = 'drop'
    ) %>% mutate(
      Lost.Trashed.Inventory = pmax(0, Total.Purchase.Rate - Cost.of.Sold.Inventory - Remaining.Value),
      Total.Gross.Profit     = Total.Sales.Amount - Cost.of.Sold.Inventory,
      Net.Profit             = Total.Gross.Profit - Total.Fees
    )
  })
  
  # Aggregate by Sales Rep
  agentSummary <- reactive({
    calculatePOData() %>% group_by(Sales.Rep) %>% summarise(
      Quantity.Purchased     = sum(Quantity.Purchased),
      Total.Purchase.Rate    = sum(Total.Purchase.Rate),
      Cost.of.Sold.Inventory = sum(Cost.of.Sold.Inventory),
      Remaining.Quantity     = sum(Remaining.Quantity),
      Remaining.Value        = sum(Remaining.Value),
      Total.Sales.Amount     = sum(Total.Sales.Amount),
      Total.Fees             = sum(Total.Fees),
      Lost.Trashed.Inventory = sum(Lost.Trashed.Inventory),
      Total.Gross.Profit     = sum(Total.Gross.Profit),
      Net.Profit             = sum(Net.Profit),
      .groups = 'drop'
    ) %>% mutate(
      across(c(Total.Purchase.Rate, Cost.of.Sold.Inventory, Remaining.Value,
               Lost.Trashed.Inventory, Total.Sales.Amount, Total.Fees,
               Total.Gross.Profit, Net.Profit), scales::dollar)
    )
  })
  
  # Render DataTables
  output$agentTable <- renderDT({
    req(dataList())
    datatable(agentSummary(), selection = 'single', options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$docTable <- renderDT({
    req(dataList(), input$agentTable_rows_selected)
    agent <- agentSummary()$Sales.Rep[input$agentTable_rows_selected]
    calculatePOData() %>% filter(Sales.Rep == agent) %>% select(-Sales.Rep) %>% mutate(
      across(c(Total.Purchase.Rate, Cost.of.Sold.Inventory, Remaining.Value,
               Lost.Trashed.Inventory, Total.Sales.Amount, Total.Fees,
               Total.Gross.Profit, Net.Profit), scales::dollar)
    ) %>% datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Download handlers
  output$exportAgentExcel <- downloadHandler(
    filename = function() paste0('Agent_Summary_', Sys.Date(), '.xlsx'),
    content  = function(file) write_xlsx(agentSummary(), file)
  )
  
  output$exportDocExcel <- downloadHandler(
    filename = function() paste0('PO_Summary_', Sys.Date(), '.xlsx'),
    content = function(file) {
      req(input$agentTable_rows_selected)
      agent <- agentSummary()$Sales.Rep[input$agentTable_rows_selected]
      doc_summary <- calculatePOData() %>% filter(Sales.Rep == agent) %>% select(-Sales.Rep)
      write_xlsx(doc_summary, file)
    }
  )
}

# Launch the app
shinyApp(ui = ui, server = server)
