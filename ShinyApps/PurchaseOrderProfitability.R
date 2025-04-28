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
      fileInput("receivedFile",  "Upload Received CSV",  accept = ".csv"),
      fileInput("inventoryFile", "Upload Inventory CSV", accept = ".csv"),
      fileInput("fulfilledFile", "Upload Fulfilled CSV", accept = ".csv")
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
  
  # ── One source of truth for inventory serials ──────────────────────
  invSerials <- reactive({
    inv <- inventory()
    
    # Try to find the exact column name first
    inventory_cols <- names(inv)
    cat("Available inventory columns:", paste(inventory_cols, collapse=", "), "\n")
    
    # Try to find Formula (Text) column with flexibility
    formula_text_col <- NULL
    
    # First, try exact match
    if ("Formula (Text)" %in% inventory_cols) {
      formula_text_col <- "Formula (Text)"
    } 
    # Next, try case-insensitive match
    else {
      possible_cols <- inventory_cols[grepl("formula.*(text)", tolower(inventory_cols))]
      if (length(possible_cols) > 0) {
        formula_text_col <- possible_cols[1]
      }
    }
    
    # If still not found, look for any column with "inventory" in the name
    if (is.null(formula_text_col)) {
      possible_cols <- inventory_cols[grepl("inventory", tolower(inventory_cols))]
      if (length(possible_cols) > 0) {
        formula_text_col <- possible_cols[1]
      }
    }
    
    # As a last resort, use the columns you mentioned
    if (is.null(formula_text_col)) {
      if ("Item Inventory Number" %in% inventory_cols) {
        formula_text_col <- "Item Inventory Number"
      } else if ("Inventory Number" %in% inventory_cols) {
        formula_text_col <- "Inventory Number"
      }
    }
    
    validate(need(!is.null(formula_text_col),
                  "Could not find a suitable serial number column in the inventory file. 
                  Please ensure it contains 'Formula (Text)', 'Item Inventory Number', or 'Inventory Number'."))
    
    cat("Using inventory column:", formula_text_col, "\n")
    inv[[formula_text_col]] |> trimws() |> unique()
  })
  
  # ── Merge received + fulfilled for fee calc ────────────────────────
  mergedData <- reactive({
    # Try to find Serial.Number column with flexibility
    rec <- received()
    ful <- fulfilled()
    
    rec_serial_col <- "Serial.Number"
    if (!rec_serial_col %in% names(rec)) {
      # Try without the dot
      if ("Serial Number" %in% names(rec)) {
        rec_serial_col <- "Serial Number"
      } else {
        possible_cols <- names(rec)[grepl("serial", tolower(names(rec)))]
        if (length(possible_cols) > 0) {
          rec_serial_col <- possible_cols[1]
        }
      }
    }
    
    ful_serial_col <- "Serial.Number"
    if (!ful_serial_col %in% names(ful)) {
      # Try without the dot
      if ("Serial Number" %in% names(ful)) {
        ful_serial_col <- "Serial Number"
      } else {
        possible_cols <- names(ful)[grepl("serial", tolower(names(ful)))]
        if (length(possible_cols) > 0) {
          ful_serial_col <- possible_cols[1]
        }
      }
    }
    
    validate(need(!is.null(rec_serial_col), "Could not find a serial number column in the received file."))
    validate(need(!is.null(ful_serial_col), "Could not find a serial number column in the fulfilled file."))
    
    cat("Using received serial column:", rec_serial_col, "\n")
    cat("Using fulfilled serial column:", ful_serial_col, "\n")
    
    # Check if Item.Rate exists in received or find alternative
    rate_col <- "Item.Rate"
    if (!rate_col %in% names(rec)) {
      if ("Item Rate" %in% names(rec)) {
        rate_col <- "Item Rate"
      } else {
        possible_cols <- names(rec)[grepl("rate", tolower(names(rec)))]
        if (length(possible_cols) > 0) {
          rate_col <- possible_cols[1]
        }
      }
    }
    
    validate(need(!is.null(rate_col), "Could not find an item rate column in the received file."))
    cat("Using rate column:", rate_col, "\n")
    
    # Rename columns for consistency if needed
    if (rec_serial_col != "Serial.Number") {
      rec$Serial.Number <- rec[[rec_serial_col]]
    }
    
    if (ful_serial_col != "Serial.Number") {
      ful$Serial.Number <- ful[[ful_serial_col]]
    }
    
    if (rate_col != "Item.Rate") {
      rec$Item.Rate <- rec[[rate_col]]
    }
    
    rec %>%
      select(Serial.Number, Item.Rate) %>%
      right_join(ful, by = "Serial.Number")   # keep all fulfilled rows
  })
  
  calculateFees <- function(df) {
    # Check if eTail.Channel exists or find alternative
    channel_col <- "eTail.Channel"
    if (!channel_col %in% names(df)) {
      if ("eTail Channel" %in% names(df)) {
        channel_col <- "eTail Channel"
      } else {
        possible_cols <- names(df)[grepl("channel|etail", tolower(names(df)))]
        if (length(possible_cols) > 0) {
          channel_col <- possible_cols[1]
        }
      }
    }
    
    if (channel_col != "eTail.Channel" && !is.null(channel_col)) {
      df$eTail.Channel <- df[[channel_col]]
    }
    
    # Check if Rate exists in df
    rate_col <- "Rate"
    if (!rate_col %in% names(df)) {
      possible_cols <- names(df)[grepl("^rate$|^price$", tolower(names(df)))]
      if (length(possible_cols) > 0) {
        rate_col <- possible_cols[1]
      }
    }
    
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
    
    # Find Sales.Rep column
    sales_rep_col <- "Sales.Rep"
    if (!sales_rep_col %in% names(rec)) {
      if ("Sales Rep" %in% names(rec)) {
        sales_rep_col <- "Sales Rep"
      } else {
        possible_cols <- names(rec)[grepl("sales.*rep|rep|agent", tolower(names(rec)))]
        if (length(possible_cols) > 0) {
          sales_rep_col <- possible_cols[1]
        }
      }
    }
    
    validate(need(!is.null(sales_rep_col), "Could not find a sales rep column in the received file."))
    
    if (sales_rep_col != "Sales.Rep") {
      rec$Sales.Rep <- rec[[sales_rep_col]]
    }
    
    # Find Document.Number column
    doc_col <- "Document.Number"
    if (!doc_col %in% names(rec)) {
      if ("Document Number" %in% names(rec)) {
        doc_col <- "Document Number"
      } else {
        possible_cols <- names(rec)[grepl("doc|purchase.*order|po", tolower(names(rec)))]
        if (length(possible_cols) > 0) {
          doc_col <- possible_cols[1]
        }
      }
    }
    
    validate(need(!is.null(doc_col), "Could not find a document number column in the received file."))
    
    if (doc_col != "Document.Number") {
      rec$Document.Number <- rec[[doc_col]]
    }
    
    merged <- calculateFees(mergedData())
    inv_serials <- invSerials()
    
    # Calculate at PO level
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
    # Now use the PO-level data to calculate agent summary by grouping
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
        # Sum up the already calculated Lost.Trashed.Inventory values from PO level
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
    
    # Use the already calculated PO data and filter for selected agent
    calculatePOData() %>%
      filter(Sales.Rep == sel_agent) %>%
      select(-Sales.Rep) %>%  # Remove the Sales.Rep column since we're already filtered
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
      
      # Use the already calculated PO data and filter for selected agent
      doc_summary <- calculatePOData() %>%
        filter(Sales.Rep == sel_agent) %>%
        select(-Sales.Rep)  # Remove the Sales.Rep column since we're already filtered
      
      write_xlsx(doc_summary, file)
    }
  )
}

# ───────────────────────────  Run the app  ───────────────────────────
shinyApp(ui = ui, server = server)
