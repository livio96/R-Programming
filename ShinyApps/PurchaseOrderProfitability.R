# ───────────────────────────  Purchase-Order Profitability Dashboard  ───────────────────────────
# ------------------------------------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(scales)
library(writexl)
library(shinyjs)

options(shiny.maxRequestSize = 100 * 1024^2)   # 100 MB uploads

urls <- list(
  received   = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20.csv",
  inventory  = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20.csv",
  fulfilled  = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20.csv",
  kits       = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20Fullfilled%20Kits.csv",
  upcharges  = "https://www.telquestintl.com/site/Purchase%20Order%20Profitability%20Dashboard/Po%20Profitability%20Upcharges.csv"
)

# ───────────────────────────────  UI  ────────────────────────────────
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', 'Calibri', Arial, sans-serif;
        font-size: 14px;
        color: #212529;
        background-color: #f8f9fa;
      }
      .container-fluid {
        max-width: 1400px;
        padding: 20px;
      }
      .card {
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        background-color: #fff;
        margin-bottom: 20px;
        border: 1px solid #e7e7e7;
      }
      .card-header {
        background-color: #f8f9fa;
        padding: 12px 15px;
        border-bottom: 1px solid #e7e7e7;
        font-weight: 600;
        font-size: 16px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .card-body {
        padding: 15px;
      }
      .btn-excel {
        background-color: #217346;
        color: white;
        border: none;
      }
      .btn-excel:hover {
        background-color: #1a5c38;
        color: white;
      }
      .modal-content {
        border-radius: 6px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }
      .modal-header {
        background: #2C3E50;
        color: #fff;
        border-bottom: none;
        padding: 15px 20px;
      }
      .modal-footer {
        border-top: none;
        padding: 15px 20px;
      }
      .btn {
        border-radius: 4px;
        font-weight: 500;
        padding: 6px 12px;
      }
      
      /* Excel-like table styling */
      table.dataTable {
        font-family: 'Segoe UI', 'Calibri', Arial, sans-serif;
        font-size: 13px;
        border-collapse: collapse;
        width: 100% !important;
      }
      table.dataTable thead th {
        background-color: #f0f0f0;
        border: 1px solid #ddd;
        border-bottom: 2px solid #ddd !important;
        padding: 8px 10px !important;
        font-weight: 600;
        color: #333;
        text-align: center;
        vertical-align: middle !important;
      }
      table.dataTable tbody td {
        border: 1px solid #e9e9e9;
        padding: 6px 10px !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: #f9f9f9;
      }
      table.dataTable tbody tr:hover {
        background-color: #f0f7fa;
      }
      
      /* Highlight profit/loss cells */
      .positive-value {
        color: #006100;
        font-weight: 500;
      }
      .negative-value {
        color: #af0000;
        font-weight: 500;
      }
      
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        padding: 0.25em 0.75em;
        margin-left: 2px;
        border-radius: 4px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background: #2C3E50 !important;
        color: white !important;
        border: 1px solid #2C3E50 !important;
      }
      .dataTables_wrapper .dataTables_filter input {
        border: 1px solid #ddd;
        border-radius: 4px;
        padding: 4px 8px;
        margin-left: 6px;
      }
      
      /* Layout to prevent horizontal scrolling */
      .table-responsive {
        overflow-x: auto;
        max-width: 100%;
      }
      
      /* Progress styling */
      .shiny-progress {
        position: fixed !important;
        top: 50% !important;
        left: 50% !important;
        width: 300px !important;
        transform: translate(-50%,-50%) !important;
        z-index: 1050;
        background: white;
        border-radius: 6px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        padding: 20px;
      }
      .shiny-progress .progress {
        height: 10px !important;
        margin-top: 10px;
        border-radius: 5px;
      }
      
      /* Header icons */
      .section-icon {
        margin-right: 8px;
        color: #217346;
      }
      
      /* Buttons with icons */
      .action-btn {
        margin-left: 5px;
      }
      .btn-icon {
        margin-right: 5px;
      }
      
      /* Dashboard title */
      .dashboard-title {
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #e7e7e7;
        color: #2C3E50;
        font-weight: 600;
      }
    "))
  ),
  uiOutput("appUI")
)

# ─────────────────────────────  SERVER  ──────────────────────────────
server <- function(input, output, session) {
  
  authenticated <- reactiveVal(FALSE)
  dataList      <- reactiveVal(NULL)
  
  # ---------- login ----------
  showLogin <- function(msg = NULL) {
    modalDialog(
      title = tagList(icon("unlock-alt"), "Secure Login"),
      if (!is.null(msg)) div(style="color:red;margin-bottom:15px;", msg),
      div(
        style = "text-align: center; margin-bottom: 20px;",
        tags$img(src = "https://www.telquestintl.com/images/TelQuestLogo.png", 
                 width = "200px", alt = "TelQuest Logo")
      ),
      passwordInput("password", "Enter Password", placeholder="••••••••"),
      footer = tagList(
        actionButton("login", "Login", class="btn-primary", 
                     icon = icon("sign-in-alt")),
        tags$small("Powered by TelQuest")
      ),
      easyClose = FALSE, fade = TRUE, size = "s"
    )
  }
  observe({ if (!authenticated()) showModal(showLogin()) })
  
  observeEvent(input$login, {
    req(input$password)
    if (input$password == "") {
      
      authenticated(TRUE); removeModal()
      
      withProgress(message = "Fetching data…", value = 0, {
        steps <- seq(0,1,length.out = length(urls) + 1)
        incProgress(steps[2], detail = "Received"  ); rec  <- read.csv(url(urls$received) , stringsAsFactors = FALSE)
        incProgress(steps[3], detail = "Inventory" ); inv  <- read.csv(url(urls$inventory), stringsAsFactors = FALSE)
        incProgress(steps[4], detail = "Fulfilled" ); ful  <- read.csv(url(urls$fulfilled), stringsAsFactors = FALSE)
        incProgress(steps[5], detail = "Kits"      ); kits <- read.csv(url(urls$kits)     , stringsAsFactors = FALSE)
        incProgress(steps[6], detail = "Up-charges"); ups  <- read.csv(url(urls$upcharges), stringsAsFactors = FALSE)
      })
      
      dataList(list(received = rec, inventory = inv, fulfilled = ful,
                    kits = kits, upcharges = ups))
      
    } else showModal(showLogin("Incorrect password, please try again."))
  })
  
  # ---------- UI after login ----------
  output$appUI <- renderUI({
    req(authenticated(), dataList())
    fluidPage(
      div(class = "dashboard-title",
          h2(icon("chart-line", class = "section-icon"), 
             "Purchase-Order Profitability Dashboard")),
      
      fluidRow(
        column(12,
               div(class = "card",
                   div(class = "card-header",
                       span(icon("user-tie", class = "section-icon"), "Purchasing Agent Summary"),
                       div(
                         downloadButton("exportAgentExcel", "Export Summary", 
                                        class = "btn-excel action-btn",
                                        icon = icon("file-excel", class = "btn-icon"))
                       )
                   ),
                   div(class = "card-body",
                       div(class = "table-responsive", DTOutput("agentTable"))
                   )
               )
        )
      ),
      
      fluidRow(
        column(12,
               div(class = "card",
                   div(class = "card-header",
                       span(icon("clipboard-list", class = "section-icon"), "Purchase Order Breakdown"),
                       div(
                         downloadButton("exportDocExcel", "Export Details", 
                                        class = "btn-excel action-btn",
                                        icon = icon("file-excel", class = "btn-icon"))
                       )
                   ),
                   div(class = "card-body",
                       div(class = "table-responsive", DTOutput("docTable"))
                   )
               )
        )
      )
    )
  })
  
  # ─────────────  Reactive datasets  ─────────────
  received     <- reactive(dataList()$received)
  inventory    <- reactive(dataList()$inventory)
  fulfilled    <- reactive(dataList()$fulfilled)
  kits         <- reactive(dataList()$kits)
  upchargesRaw <- reactive(dataList()$upcharges)
  allFulfilled <- reactive(bind_rows(fulfilled(), kits()))
  
  # ─────────────  Clean Up-charges (Internal ID)  ─────────────
  cleanUpcharges <- reactive({
    ups <- upchargesRaw()
    idCol  <- names(ups)[grepl("internal.*id", tolower(names(ups)))][1]
    qtyCol <- names(ups)[grepl("quantity",     tolower(names(ups)))][1]
    amtCol <- names(ups)[grepl("total.*upcharge", tolower(names(ups)))][1]
    req(idCol, qtyCol, amtCol, "Up-charges: required columns not found.")
    ups %>%
      transmute(
        Internal.ID    = as.character(.data[[idCol]]),                     # force char
        Quantity       = as.numeric(gsub("[^0-9.-]", "", .data[[qtyCol]])),
        Total.Upcharge = as.numeric(gsub("[^0-9.-]", "", .data[[amtCol]]))
      ) %>%
      group_by(Internal.ID) %>%
      summarise(
        Serial.Upcharge = ifelse(sum(Quantity, na.rm = TRUE) > 0,
                                 sum(Total.Upcharge, na.rm = TRUE) /
                                   sum(Quantity,       na.rm = TRUE), 0),
        .groups = "drop"
      )
  })
  
  # ─────────────  Inventory serials on hand  ─────────────
  invSerials <- reactive({
    inv <- inventory(); cols <- names(inv)
    col <- case_when(
      "Formula (Text)"                %in% cols ~ "Formula (Text)",
      any(grepl("formula.*text",tolower(cols))) ~ cols[grepl("formula.*text",tolower(cols))][1],
      "Item Inventory Number"         %in% cols ~ "Item Inventory Number",
      "Inventory Number"              %in% cols ~ "Inventory Number",
      TRUE                                           ~ NA_character_)
    req(!is.na(col),"Serial column not found in Inventory.")
    unique(trimws(inv[[col]]))
  })
  
  # ─────────────  Marketplace fees helper  ─────────────
  calculateFees <- function(df){
    chan <- names(df)[grepl("channel|etail",tolower(names(df)))][1]
    pr   <- names(df)[grepl("^rate$|^price$",tolower(names(df)))][1]
    if(!is.null(chan)) df$eTail.Channel <- df[[chan]]
    if(!is.null(pr))   df$Rate          <- df[[pr]]
    df %>% mutate(
      Fees = case_when(
        eTail.Channel %in% c("Amazon","Newegg","Newegg Business","Walmart","eBay") ~ 0.14*Rate,
        eTail.Channel == "Sales Team" & Rate > Item.Rate                           ~ 0.14*(Rate-Item.Rate),
        TRUE                                                                       ~ 0)
    )
  }
  
  # ─────────────  Merge Received ↔ Fulfilled/Kits (serial)  ─────────────
  mergedData <- reactive({
    rec <- received(); ful <- allFulfilled()
    recSN <- names(rec)[grepl("serial",tolower(names(rec)))][1]
    fulSN <- names(ful)[grepl("serial",tolower(names(ful)))][1]
    rate  <- names(rec)[grepl("rate",  tolower(names(rec)))][1]
    req(recSN,fulSN,rate,"Serial or rate column missing.")
    rec$Serial.Number <- rec[[recSN]]; rec$Item.Rate <- rec[[rate]]
    ful$Serial.Number <- ful[[fulSN]]
    right_join(select(rec, Serial.Number, Item.Rate),
               ful,
               by = "Serial.Number")
  })
  
  # ─────────────  Core PO-level calculation (grouped by Document Number)  ─────────────
  calculatePOData <- reactive({
    
    rec <- received() %>% ungroup()
    
    recSN <- names(rec)[grepl("serial",tolower(names(rec)))][1]
    rate  <- names(rec)[grepl("rate",  tolower(names(rec)))][1]
    idCol <- names(rec)[grepl("internal.*id",tolower(names(rec)))][1]
    docCol<- names(rec)[grepl("doc|purchase.*order|po",tolower(names(rec)))][1]
    
    req(recSN,rate,idCol,docCol,"Essential columns missing in Received.")
    
    rec$Serial.Number   <- rec[[recSN]]
    rec$Item.Rate       <- rec[[rate]]
    rec$Internal.ID     <- as.character(rec[[idCol]])      # char for join
    rec$Document.Number <- rec[[docCol]]
    
    # attach up-charge per serial
    rec <- left_join(rec, cleanUpcharges(), by = "Internal.ID",
                     relationship = "many-to-one")
    rec$Serial.Upcharge[is.na(rec$Serial.Upcharge)] <- 0
    
    soldSN <- unique(mergedData()$Serial.Number)
    rec$Upcharge.Sold <- ifelse(rec$Serial.Number %in% soldSN,
                                rec$Serial.Upcharge, 0)
    
    merged <- calculateFees(mergedData())
    invSN  <- invSerials()
    salesCol <- names(rec)[grepl("sales.*rep|rep|agent",tolower(names(rec)))][1]
    rec$Sales.Rep <- rec[[salesCol]]
    
    # -------- aggregate by Sales Rep + Document Number (Internal ID not shown) --------
    rec %>%
      group_by(Sales.Rep, Document.Number) %>%
      summarise(
        Quantity.Purchased     = n(),
        Total.Purchase.Rate    = sum(Item.Rate, na.rm = TRUE),
        Cost.of.Sold.Inventory = sum(Item.Rate[Serial.Number %in% soldSN], na.rm = TRUE),
        Remaining.Quantity     = sum(Serial.Number %in% invSN),
        Remaining.Value        = sum(Item.Rate[Serial.Number %in% invSN], na.rm = TRUE),
        Upcharges              = sum(Upcharge.Sold, na.rm = TRUE),
        Total.Sales.Amount     = sum(merged$Rate[merged$Serial.Number %in% Serial.Number], na.rm = TRUE),
        Total.Fees             = sum(merged$Fees[merged$Serial.Number %in% Serial.Number], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Lost.Trashed.Inventory = pmax(0, Total.Purchase.Rate -
                                        Cost.of.Sold.Inventory -
                                        Remaining.Value),
        
        Total.Gross.Profit = Total.Sales.Amount - Cost.of.Sold.Inventory,
        Net.Profit= Total.Gross.Profit - Total.Fees - Upcharges - Lost.Trashed.Inventory
      )
    
  })
  
  # ─────────────  Agent summary  ─────────────
  agentSummary <- reactive({
    calculatePOData() %>%
      group_by(Sales.Rep) %>%
      summarise(
        Quantity.Purchased     = sum(Quantity.Purchased),
        Total.Purchase.Rate    = sum(Total.Purchase.Rate),
        Cost.of.Sold.Inventory = sum(Cost.of.Sold.Inventory),
        Remaining.Quantity     = sum(Remaining.Quantity),
        Remaining.Value        = sum(Remaining.Value),
        Upcharges              = sum(Upcharges),
        Total.Sales.Amount     = sum(Total.Sales.Amount),
        Total.Fees             = sum(Total.Fees),
        Lost.Trashed.Inventory = sum(Lost.Trashed.Inventory),
        Total.Gross.Profit     = sum(Total.Gross.Profit),
        Net.Profit             = sum(Net.Profit),
        .groups = "drop"
      )
  })
  
  # ─────────────  Format dollar values with colors  ─────────────
  formatMoneyWithColor <- function(x) {
    ifelse(x >= 0, 
           paste0("<span class='positive-value'>", scales::dollar(x), "</span>"),
           paste0("<span class='negative-value'>", scales::dollar(x), "</span>"))
  }
  
  # ─────────────  Tables  ─────────────
  output$agentTable <- renderDT({
    dt <- agentSummary()
    
    # Create a copy of the data for display purposes
    display_dt <- dt
    
    # Format monetary columns with dollar signs and coloring
    money_cols <- c("Total.Purchase.Rate", "Cost.of.Sold.Inventory", "Remaining.Value",
                    "Upcharges", "Total.Sales.Amount", "Total.Fees",
                    "Lost.Trashed.Inventory", "Total.Gross.Profit", "Net.Profit")
    
    # Format display data
    for(col in money_cols) {
      display_dt[[col]] <- formatMoneyWithColor(dt[[col]])
    }
    
    # Rename columns to more readable format
    names(display_dt) <- gsub("\\.", " ", names(display_dt))
    
    datatable(display_dt,
              selection = "single",
              options = list(
                pageLength = 15,
                autoWidth = TRUE,
                scrollX = FALSE,
                dom = '<"top"f>rt<"bottom"ip>',
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(width = '150px', targets = 0)
                ),
                lengthMenu = list(c(10, 15, 25, 50, -1), c('10', '15', '25', '50', 'All'))
              ),
              escape = FALSE,
              rownames = FALSE,
              class = "cell-border stripe") %>%
      formatStyle(columns = names(display_dt), fontSize = '13px')
  })
  
  output$docTable <- renderDT({
    req(input$agentTable_rows_selected)
    
    # Get the selected sales rep name
    agent_names <- agentSummary()$Sales.Rep
    agent <- agent_names[input$agentTable_rows_selected]
    
    dt <- calculatePOData() %>% filter(Sales.Rep == agent) %>% select(-Sales.Rep)
    
    # Create a copy for display
    display_dt <- dt
    
    # Format money columns with colors
    money_cols <- c("Total.Purchase.Rate", "Cost.of.Sold.Inventory", "Remaining.Value",
                    "Upcharges", "Total.Sales.Amount", "Total.Fees",
                    "Lost.Trashed.Inventory", "Total.Gross.Profit", "Net.Profit")
    
    for(col in money_cols) {
      display_dt[[col]] <- formatMoneyWithColor(dt[[col]])
    }
    
    # Rename columns to more readable format
    names(display_dt) <- gsub("\\.", " ", names(display_dt))
    
    datatable(display_dt,
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                scrollX = FALSE,
                dom = '<"top"f>rt<"bottom"ip>',
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(width = '120px', targets = 0)
                ),
                lengthMenu = list(c(10, 15, 25, 50, -1), c('10', '15', '25', '50', 'All'))
              ),
              escape = FALSE,
              rownames = FALSE,
              class = "cell-border stripe") %>%
      formatStyle(columns = names(display_dt), fontSize = '13px')
  })
  
  # ─────────────  Downloads  ─────────────
  output$exportAgentExcel <- downloadHandler(
    filename = function() paste0("Agent_Summary_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      # Get raw data for Excel (not HTML formatted)
      data <- agentSummary()
      write_xlsx(data, file)
    }
  )
  
  output$exportDocExcel <- downloadHandler(
    filename = function() paste0("PO_Details_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      agent <- agentSummary()$Sales.Rep[input$agentTable_rows_selected]
      # Get raw data for Excel
      data <- calculatePOData() %>% filter(Sales.Rep == agent)
      write_xlsx(data, file)
    }
  )
}

# ─────────────────────────────  LAUNCH  ─────────────────────────────
shinyApp(ui = ui, server = server)
