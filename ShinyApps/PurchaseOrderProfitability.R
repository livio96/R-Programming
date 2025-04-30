# ───────────────────────────  Purchase-Order Profitability Dashboard  ───────────────────────────
# New: "Lost / Trashed Inventory"  ≙  max(0, Total.Purchase.Rate − Cost.of.Sold.Inventory − Remaining.Value)
# -------------------------------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(scales)      # dollar()
library(writexl)     # write_xlsx()

options(shiny.maxRequestSize = 100 * 1024^2)   # 100 MB uploads

# ────────────────────────────────  UI  ────────────────────────────────
ui <- fluidPage(
  titlePanel("Purchase Order Profitability Dashboard"),
  fluidRow(
    column(
      6,
      fileInput("receivedFile",  "Upload Received CSV",             accept = ".csv"),
      fileInput("inventoryFile", "Upload Inventory CSV",            accept = ".csv"),
      fileInput("fulfilledFile", "Upload Fulfilled CSV",            accept = ".csv"),
      fileInput("kitsFile",      "Upload Fulfilled-Kits CSV",       accept = ".csv")
    ),
    column(
      6,
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

# ─────────────────────────────  SERVER  ──────────────────────────────
server <- function(input, output, session) {
  
  # ── Reactive uploads ───────────────────────────────────────────────
  received  <- reactive({ req(input$receivedFile);  read.csv(input$receivedFile$datapath) })
  inventory <- reactive({ req(input$inventoryFile); read.csv(input$inventoryFile$datapath) })
  fulfilled <- reactive({ req(input$fulfilledFile); read.csv(input$fulfilledFile$datapath) })
  kits      <- reactive({
    if (is.null(input$kitsFile)) data.frame() else read.csv(input$kitsFile$datapath)
  })
  
  # Combine fulfilled + kits
  allFulfilled <- reactive(bind_rows(fulfilled(), kits()))
  
  # ── One source of truth for inventory serials ──────────────────────
  invSerials <- reactive({
    inv <- inventory()
    inventory_cols <- names(inv)
    
    formula_text_col <- dplyr::case_when(
      "Formula (Text)"                 %in% inventory_cols ~ "Formula (Text)",
      any(grepl("formula.*text", tolower(inventory_cols))) ~ inventory_cols[grepl("formula.*text", tolower(inventory_cols))][1],
      any(grepl("inventory",   tolower(inventory_cols)))   ~ inventory_cols[grepl("inventory",   tolower(inventory_cols))][1],
      "Item Inventory Number"          %in% inventory_cols ~ "Item Inventory Number",
      "Inventory Number"               %in% inventory_cols ~ "Inventory Number",
      TRUE                                                ~ NA_character_
    )
    
    validate(need(!is.na(formula_text_col),
                  "Could not find a suitable serial number column in the inventory file."))
    inv[[formula_text_col]] |> trimws() |> unique()
  })
  
  # ── Merge received + allFulfilled for fee calc ─────────────────────
  mergedData <- reactive({
    rec <- received()
    ful <- allFulfilled()
    
    rec_serial_col <- if ("Serial.Number" %in% names(rec)) "Serial.Number"
                      else if ("Serial Number" %in% names(rec)) "Serial Number"
                      else names(rec)[grepl("serial", tolower(names(rec)))][1]
    
    ful_serial_col <- if ("Serial.Number" %in% names(ful)) "Serial.Number"
                      else if ("Serial Number" %in% names(ful)) "Serial Number"
                      else names(ful)[grepl("serial", tolower(names(ful)))][1]
    
    validate(need(!is.na(rec_serial_col), "Could not find a serial number column in the received file."))
    validate(need(!is.na(ful_serial_col), "Could not find a serial number column in the fulfilled files."))
    
    rate_col <- if ("Item.Rate" %in% names(rec)) "Item.Rate"
                else if ("Item Rate" %in% names(rec)) "Item Rate"
                else names(rec)[grepl("rate", tolower(names(rec)))][1]
    
    validate(need(!is.na(rate_col), "Could not find an item rate column in the received file."))
    
    if (rec_serial_col != "Serial.Number") rec$Serial.Number <- rec[[rec_serial_col]]
    if (ful_serial_col != "Serial.Number") ful$Serial.Number <- ful[[ful_serial_col]]
    if (rate_col        != "Item.Rate")    rec$Item.Rate     <- rec[[rate_col]]
    
    rec %>%
      select(Serial.Number, Item.Rate) %>%
      right_join(ful, by = "Serial.Number")   # keep all fulfilled rows
  })
  
  calculateFees <- function(df) {
    channel_col <- if ("eTail.Channel" %in% names(df)) "eTail.Channel"
                   else if ("eTail Channel" %in% names(df)) "eTail Channel"
                   else names(df)[grepl("channel|etail", tolower(names(df)))][1]
    
    if (channel_col != "eTail.Channel" && !is.null(channel_col)) {
      df$eTail.Channel <- df[[channel_col]]
    }
    
    rate_col <- if ("Rate" %in% names(df)) "Rate"
                else names(df)[grepl("^rate$|^price$", tolower(names(df)))][1]
    
    if (rate_col != "Rate" && !is.null(rate_col)) {
      df$Rate <- df[[rate_col]]
    }
    
    df %>%
      mutate(
        Fees = case_when(
          eTail.Channel %in% c("Amazon", "Newegg", "Newegg Business",
                               "Walmart", "eBay") ~ 0.14 * Rate,
          is.na(eTail.Channel) | eTail.Channel == ""                     ~ 0,
          eTail.Channel == "Sales Team" & Item.Rate >= Rate              ~ 0,
          eTail.Channel == "Sales Team" & Rate > Item.Rate               ~ 0.14 * (Rate - Item.Rate),
          TRUE                                                           ~ 0
        )
      )
  }
  
  # ── First calculate PO level data ─────────────────────────────
  calculatePOData <- reactive({
    rec <- received()
    
    sales_rep_col <- if ("Sales.Rep" %in% names(rec)) "Sales.Rep"
                     else if ("Sales Rep" %in% names(rec)) "Sales Rep"
                     else names(rec)[grepl("sales.*rep|rep|agent", tolower(names(rec)))][1]
    
    validate(need(!is.na(sales_rep_col), "Could not find a sales rep column in the received file."))
    if (sales_rep_col != "Sales.Rep") rec$Sales.Rep <- rec[[sales_rep_col]]
    
    doc_col <- if ("Document.Number" %in% names(rec)) "Document.Number"
               else if ("Document Number" %in% names(rec)) "Document Number"
               else names(rec)[grepl("doc|purchase.*order|po", tolower(names(rec)))][1]
    
    validate(need(!is.na(doc_col), "Could not find a document number column in the received file."))
    if (doc_col != "Document.Number") rec$Document.Number <- rec[[doc_col]]
    
    merged      <- calculateFees(mergedData())
    inv_serials <- invSerials()
    
    rec %>%
      group_by(Sales.Rep, Document.Number) %>%
      summarise(
        Quantity.Purchased      = n(),
        Total.Purchase.Rate     = sum(Item.Rate, na.rm = TRUE),
        Cost.of.Sold.Inventory  = sum(Item.Rate[Serial.Number %in% merged$Serial.Number], na.rm = TRUE),
        Remaining.Quantity      = sum(Serial.Number %in% inv_serials),
        Remaining.Value         = sum(Item.Rate[Serial.Number %in% inv_serials], na.rm = TRUE),
        Total.Sales.Amount      = sum(merged$Rate[merged$Serial.Number %in% Serial.Number], na.rm = TRUE),
        Total.Fees              = sum(merged$Fees[merged$Serial.Number %in% Serial.Number], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Lost.Trashed.Inventory  = pmax(0, Total.Purchase.Rate - Cost.of.Sold.Inventory - Remaining.Value),
        Total.Gross.Profit      = Total.Sales.Amount - Cost.of.Sold.Inventory,
        Net.Profit              = Total.Gross.Profit - Total.Fees
      )
  })
  
  # ── Sales-agent summary ────────────────────────────────────────────
  agentSummary <- reactive({
    calculatePOData() %>%
      group_by(Sales.Rep) %>%
      summarise(
        Quantity.Purchased      = sum(Quantity.Purchased),
        Total.Purchase.Rate     = sum(Total.Purchase.Rate),
        Cost.of.Sold.Inventory  = sum(Cost.of.Sold.Inventory),
        Remaining.Quantity      = sum(Remaining.Quantity),
        Remaining.Value         = sum(Remaining.Value),
        Total.Sales.Amount      = sum(Total.Sales.Amount),
        Total.Fees              = sum(Total.Fees),
        Lost.Trashed.Inventory  = sum(Lost.Trashed.Inventory),
        Total.Gross.Profit      = sum(Total.Gross.Profit),
        Net.Profit              = sum(Net.Profit),
        .groups = "drop"
      ) %>%
      mutate(
        across(c(Total.Purchase.Rate, Cost.of.Sold.Inventory, Remaining.Value,
                 Lost.Trashed.Inventory, Total.Sales.Amount, Total.Fees,
                 Total.Gross.Profit, Net.Profit), dollar)
      )
  })
  
  output$agentTable <- renderDT({
    datatable(agentSummary(), selection = "single",
              options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # ── PO (document) breakdown per selected agent ─────────────────────
  output$docTable <- renderDT({
    req(input$agentTable_rows_selected)
    sel_agent <- agentSummary()$Sales.Rep[input$agentTable_rows_selected]
    
    calculatePOData() %>%
      filter(Sales.Rep == sel_agent) %>%
      select(-Sales.Rep) %>%
      mutate(
        across(c(Total.Purchase.Rate, Cost.of.Sold.Inventory, Remaining.Value,
                 Lost.Trashed.Inventory, Total.Sales.Amount, Total.Fees,
                 Total.Gross.Profit, Net.Profit), dollar)
      ) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ── Excel exports ──────────────────────────────────────────────────
  output$exportAgentExcel <- downloadHandler(
    filename = function() paste0("Agent_Summary_", Sys.Date(), ".xlsx"),
    content  = function(file) write_xlsx(agentSummary(), file)
  )
  
  output$exportDocExcel <- downloadHandler(
    filename = function() paste0("PO_Summary_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(input$agentTable_rows_selected)
      sel_agent <- agentSummary()$Sales.Rep[input$agentTable_rows_selected]
      
      doc_summary <- calculatePOData() %>%
        filter(Sales.Rep == sel_agent) %>%
        select(-Sales.Rep)
      
      write_xlsx(doc_summary, file)
    }
  )
}

# ───────────────────────────  Run the app  ───────────────────────────
shinyApp(ui = ui, server = server)
