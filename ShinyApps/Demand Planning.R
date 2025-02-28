# app.R

library(shiny)
library(DT)         # For renderDT / datatable
library(openxlsx)   # For Excel export
library(dplyr)      # For joins and data wrangling

ui <- fluidPage(
  titlePanel("Restock with On Order, Watch List, and Cisco Refresh (Union Items)"),
  
  tabsetPanel(id = "tabs",
              
              # STEP 1: UPLOAD FILES
              tabPanel(
                "1. Upload Data",
                sidebarLayout(
                  sidebarPanel(
                    # Main Inventory (Required)
                    fileInput("file_inventory", "Upload Main Inventory CSV",
                              accept = c(".csv")),
                    
                    tags$hr(),
                    
                    # Watch List (Optional)
                    fileInput("file_watchlist", "Optional: Upload Watch List CSV",
                              accept = c(".csv")),
                    
                    tags$hr(),
                    
                    # Cisco Refresh (Optional)
                    fileInput("file_cisco_refresh", "Optional: Upload Cisco Refresh CSV",
                              accept = c(".csv")),
                    
                    tags$hr(),
                    actionButton("go_step2", "Go to Step 2")
                  ),
                  mainPanel(
                    h4("Instructions:"),
                    tags$ol(
                      tags$li("Upload the Main Inventory CSV (required). Must contain at least: 
                     Item (ID), Current Stock, On Order, Sales (30 days), Sales (365 days)."),
                      tags$li("Optionally upload a Watch List CSV (just needs an Item column)."),
                      tags$li("Optionally upload a Cisco Refresh CSV with columns: 
                     Cisco Part Number, Refresh Quantity, and Refresh Cost (will be multiplied by 0.02475)."),
                      tags$li("Click 'Go to Step 2' to map columns.")
                    )
                  )
                )
              ),
              
              # STEP 2: PREVIEW & COLUMN MAPPING
              tabPanel(
                "2. Preview / Column Mapping",
                sidebarLayout(
                  sidebarPanel(
                    h4("Main Inventory Mapping"),
                    uiOutput("col_item_ui"),
                    uiOutput("col_stock_ui"),
                    uiOutput("col_on_order_ui"),
                    uiOutput("col_30_ui"),
                    uiOutput("col_365_ui"),
                    
                    tags$hr(),
                    h4("Cisco Refresh Mapping (Optional)"),
                    uiOutput("col_cisco_part_ui"),
                    uiOutput("col_cisco_qty_ui"),
                    uiOutput("col_cisco_cost_ui"),
                    
                    tags$hr(),
                    actionButton("go_step3", "Go to Step 3")
                  ),
                  mainPanel(
                    h4("Preview: Main Inventory"),
                    tableOutput("preview_table"),
                    
                    tags$hr(),
                    h4("Preview: Cisco Refresh (Optional)"),
                    tableOutput("preview_cisco_refresh")
                  )
                )
              ),
              
              # STEP 3: CONFIGURE & GENERATE RECOMMENDATIONS
              tabPanel(
                "3. Restock Recommendations",
                sidebarLayout(
                  sidebarPanel(
                    # Usage basis
                    radioButtons("usage_basis", 
                                 "Usage Calculation Based On:", 
                                 choices = c("Last 30 Days" = "30", 
                                             "Last 365 Days" = "365")),
                    numericInput("lead_time", 
                                 "Lead Time (days):", 
                                 value = 30, min = 1),
                    numericInput("safety_stock_days", 
                                 "Safety Stock (days):", 
                                 value = 15, min = 0),
                    
                    # Filters
                    checkboxInput("only_reorder_needed", 
                                  "Show only items that need reorder?", 
                                  value = TRUE),
                    checkboxInput("only_watch_list",
                                  "Show only items on the watch list?",
                                  value = FALSE),
                    checkboxInput("only_cisco_refresh",
                                  "Show only items with Cisco refresh quantity > 0?",
                                  value = FALSE),
                    
                    textInput("item_filter", 
                              "Filter by Item Name/ID:", 
                              value = ""),
                    
                    tags$hr(),
                    actionButton("calculate_restock", "Calculate Reorder Quantities"),
                    
                    tags$hr(),
                    downloadButton("download_excel", "Download Excel")
                  ),
                  mainPanel(
                    h4("Recommended Reorder (plus Cisco/Watch Info)"),
                    DTOutput("restock_table")  # Using DT for an interactive table
                  )
                )
              )
  )
)

server <- function(input, output, session) {
  
  #
  # 1) READ MAIN INVENTORY (REQUIRED)
  #
  inventory_data <- reactive({
    req(input$file_inventory)
    read.csv(input$file_inventory$datapath, stringsAsFactors = FALSE)
  })
  
  #
  # 1B) WATCH LIST (OPTIONAL)
  #
  watchlist_data <- reactive({
    if (is.null(input$file_watchlist)) {
      return(NULL)
    }
    read.csv(input$file_watchlist$datapath, stringsAsFactors = FALSE)
  })
  
  #
  # 1C) CISCO REFRESH (OPTIONAL)
  #
  ciscoRefresh_data <- reactive({
    if (is.null(input$file_cisco_refresh)) {
      return(NULL)
    }
    read.csv(input$file_cisco_refresh$datapath, stringsAsFactors = FALSE)
  })
  
  #
  # NAVIGATION: Step 1 -> Step 2
  #
  observeEvent(input$go_step2, {
    updateTabsetPanel(session, inputId = "tabs", selected = "2. Preview / Column Mapping")
  })
  
  #
  # NAVIGATION: Step 2 -> Step 3
  #
  observeEvent(input$go_step3, {
    updateTabsetPanel(session, inputId = "tabs", selected = "3. Restock Recommendations")
  })
  
  #
  # 2A) DYNAMIC UI FOR MAIN INVENTORY
  #
  output$col_item_ui <- renderUI({
    req(inventory_data())
    selectInput("col_item", "Item Column", choices = names(inventory_data()))
  })
  
  output$col_stock_ui <- renderUI({
    req(inventory_data())
    selectInput("col_stock", "Current Stock Column", choices = names(inventory_data()))
  })
  
  output$col_on_order_ui <- renderUI({
    req(inventory_data())
    selectInput("col_on_order", "On Order Column", choices = names(inventory_data()))
  })
  
  output$col_30_ui <- renderUI({
    req(inventory_data())
    selectInput("col_30", "Sales Last 30 Days", choices = names(inventory_data()))
  })
  
  output$col_365_ui <- renderUI({
    req(inventory_data())
    selectInput("col_365", "Sales Last 365 Days", choices = names(inventory_data()))
  })
  
  #
  # 2B) PREVIEW THE MAIN INVENTORY
  #
  output$preview_table <- renderTable({
    req(inventory_data())
    head(inventory_data(), 10)
  })
  
  #
  # 2C) DYNAMIC UI FOR CISCO REFRESH
  #
  output$col_cisco_part_ui <- renderUI({
    req(ciscoRefresh_data())
    selectInput("col_cisco_part", "Cisco Refresh Part Number", choices = names(ciscoRefresh_data()))
  })
  
  output$col_cisco_qty_ui <- renderUI({
    req(ciscoRefresh_data())
    selectInput("col_cisco_qty", "Cisco Refresh Quantity", choices = names(ciscoRefresh_data()))
  })
  
  output$col_cisco_cost_ui <- renderUI({
    req(ciscoRefresh_data())
    selectInput("col_cisco_cost", "Cisco Refresh Cost", choices = names(ciscoRefresh_data()))
  })
  
  output$preview_cisco_refresh <- renderTable({
    req(ciscoRefresh_data())
    head(ciscoRefresh_data(), 10)
  })
  
  #
  # 3) CALCULATE THE FINAL TABLE (UNION of MAIN, WATCH, REFRESH)
  #
  restock_calculation <- eventReactive(input$calculate_restock, {
    # Must have main inventory mappings
    req(inventory_data(),
        input$col_item, input$col_stock, input$col_on_order,
        input$col_30, input$col_365,
        input$usage_basis, input$lead_time)
    
    # 1) Main Inventory data frame
    df_main <- inventory_data()
    # Extract mapped columns
    df_main_mapped <- data.frame(
      Item         = df_main[[input$col_item]],
      CurrentStock = as.numeric(df_main[[input$col_stock]]),
      OnOrder      = as.numeric(df_main[[input$col_on_order]]),
      Sales30      = as.numeric(df_main[[input$col_30]]),
      Sales365     = as.numeric(df_main[[input$col_365]])
    )
    
    # 2) Watch List data
    df_watch <- watchlist_data()
    watch_items <- character(0)
    if (!is.null(df_watch)) {
      # We'll assume the watch list's first column contains the Item ID
      watch_items <- as.character(df_watch[[1]])
    }
    
    # 3) Cisco Refresh data
    df_cisco <- ciscoRefresh_data()
    df_cisco_mapped <- NULL
    if (!is.null(df_cisco) && 
        !is.null(input$col_cisco_part) && 
        !is.null(input$col_cisco_qty) &&
        !is.null(input$col_cisco_cost)) {
      
      df_cisco_mapped <- data.frame(
        CiscoPart    = as.character(df_cisco[[input$col_cisco_part]]),
        RefreshQty   = as.numeric(df_cisco[[input$col_cisco_qty]]),
        # Multiply cost by 0.02475
        RefreshCost  = as.numeric(df_cisco[[input$col_cisco_cost]]) * 0.02475
      )
    }
    
    # --------------------------------------------------------------------
    # CREATE A MASTER LIST OF ALL ITEMS:
    #   - from main inventory
    #   - from watch list
    #   - from cisco refresh
    # --------------------------------------------------------------------
    all_main_items   = df_main_mapped$Item
    all_watch_items  = watch_items
    all_cisco_items  = if (!is.null(df_cisco_mapped)) df_cisco_mapped$CiscoPart else character(0)
    
    all_items <- union(all_main_items, union(all_watch_items, all_cisco_items))
    
    # Build a base data frame with one row per unique item
    all_df <- data.frame(Item = all_items, stringsAsFactors = FALSE)
    
    # --------------------------------------------------------------------
    # LEFT JOIN MAIN INVENTORY
    # If item not in main inventory, get NAs -> We'll set them to 0
    # --------------------------------------------------------------------
    all_df <- all_df %>%
      left_join(df_main_mapped, by = "Item")
    
    # Replace NA with 0 for numeric columns in main inventory
    for (coln in c("CurrentStock","OnOrder","Sales30","Sales365")) {
      all_df[[coln]][is.na(all_df[[coln]])] <- 0
    }
    
    # --------------------------------------------------------------------
    # Usage Calculation & Reorder for items that actually have main inventory data
    # We'll detect that if Sales30 or Sales365 > 0 (or we can keep 0 if not in main).
    # --------------------------------------------------------------------
    # Decide usage basis
    if (input$usage_basis == "30") {
      # daily usage = Sales30/30
      all_df$DailyUsage <- all_df$Sales30 / 30
    } else {
      # daily usage = Sales365/365
      all_df$DailyUsage <- all_df$Sales365 / 365
    }
    
    # TargetStock = DailyUsage * (leadTime + safetyStockDays)
    all_df$TargetStock <- all_df$DailyUsage * (input$lead_time + input$safety_stock_days)
    
    # ReorderQty = max(0, round(TargetStock - (CurrentStock + OnOrder)))
    # For items not in main inventory (which have 0 for stock, etc.), this will be 0 if usage is also 0
    reorder_vec <- all_df$TargetStock - (all_df$CurrentStock + all_df$OnOrder)
    all_df$ReorderQty <- pmax(0, round(reorder_vec))
    
    # --------------------------------------------------------------------
    # WATCH LIST TAG
    # --------------------------------------------------------------------
    all_df$OnWatchList <- ifelse(all_df$Item %in% watch_items, "Yes", "No")
    
    # --------------------------------------------------------------------
    # CISCO REFRESH JOIN
    # If we have cisco data, left_join on Item = CiscoPart
    # --------------------------------------------------------------------
    if (!is.null(df_cisco_mapped)) {
      all_df <- all_df %>%
        left_join(df_cisco_mapped, by = c("Item" = "CiscoPart"))
      
      # Fill NA for numeric columns
      all_df$RefreshQty[is.na(all_df$RefreshQty)] <- 0
      all_df$RefreshCost[is.na(all_df$RefreshCost)] <- 0
    } else {
      # No cisco data
      all_df$RefreshQty  <- 0
      all_df$RefreshCost <- 0
    }
    
    # all_df is now our big table with union of items, inventory data (if any),
    # watch list flags, cisco refresh columns, reorder calculations, etc.
    
    all_df
  })
  
  #
  # REACTIVE FILTERS
  #
  filtered_data <- reactive({
    req(restock_calculation())
    data <- restock_calculation()
    
    # (1) Only reorder needed?
    if (input$only_reorder_needed) {
      data <- subset(data, ReorderQty > 0)
    }
    
    # (2) Only watch list?
    if (input$only_watch_list) {
      data <- subset(data, OnWatchList == "Yes")
    }
    
    # (3) Only Cisco refresh > 0
    if (input$only_cisco_refresh) {
      data <- subset(data, RefreshQty > 0)
    }
    
    # (4) Partial item filter
    filter_text <- trimws(input$item_filter)
    if (nzchar(filter_text)) {
      data <- subset(data, grepl(tolower(filter_text), tolower(Item)))
    }
    
    data
  })
  
  #
  # DISPLAY RESULTS WITH DT
  #
  output$restock_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(),
              options = list(pageLength = 10,
                             autoWidth = TRUE))
  })
  
  #
  # DOWNLOAD EXCEL
  #
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("restock_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      data_to_save <- filtered_data()
      
      wb <- createWorkbook()
      addWorksheet(wb, "Restock")
      writeData(wb, "Restock", data_to_save)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
