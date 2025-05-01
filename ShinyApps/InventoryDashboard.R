library(shiny)
library(httr)
library(jsonlite)
library(openssl)
library(digest)
library(DT)
library(bslib)   # For modern Bootstrap themes
library(shinyjs) # For enhanced interactivity

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#217346"  # Excel-green accent
  ),
  
  ## ── Global CSS / fonts / icons ───────────────────────────────────────────────
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    ),
    tags$style(HTML("
      /*  ---- base look ---- */
      body             { font-family:'Segoe UI','Calibri',Arial,sans-serif; font-size:15px;
                         color:#212529; background:#f8f9fa; }
      .container-fluid { max-width:1400px; padding:20px; }

      /*  ---- cards ---- */
      .card            { border-radius:4px; box-shadow:0 2px 4px rgba(0,0,0,0.05);
                         background:#fff; margin-bottom:20px; border:1px solid #e7e7e7; }
      .card-header     { background:#f8f9fa; padding:12px 15px; border-bottom:1px solid #e7e7e7;
                         font-weight:600; font-size:16px; display:flex; justify-content:space-between; }

      /*  ---- Excel-look buttons ---- */
      .btn-excel       { background:#217346; color:white; border:none; }
      .btn-excel:hover { background:#1a5c38; color:white; }

      /*  ---- DataTable -> Excel aesthetic ---- */
      table.dataTable  { font-family:'Segoe UI','Calibri',Arial,sans-serif; font-size:15px;
                         border-collapse:collapse; width:100%!important; }
      table.dataTable thead th {
        background:#f0f0f0; border:1px solid #ddd; border-bottom:2px solid #ddd!important;
        padding:8px 10px!important; font-weight:600; color:#333; text-align:center; }
      table.dataTable tbody td { border:1px solid #e9e9e9; padding:6px 10px!important; }
      table.dataTable tbody tr:nth-child(even) { background:#f9f9f9; }
      table.dataTable tbody tr:hover           { background:#f0f7fa; }

      .dataTables_wrapper .dataTables_paginate .paginate_button      { padding:.25em .75em; margin-left:2px; border-radius:4px; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current { background:#217346!important; color:white!important; border:1px solid #217346!important; }
      .dataTables_wrapper .dataTables_filter input { border:1px solid #ddd; border-radius:4px; padding:4px 8px; margin-left:6px; }

      /*  ---- layout ---- */
      .table-responsive { overflow-x:auto; max-width:100%; }

      /*  ---- header ---- */
      .dashboard-header { display:flex; align-items:center; padding:15px 0; border-bottom:2px solid #e7e7e7; margin-bottom:20px; }
      .dashboard-logo   { height:60px; margin-right:20px; }
      .dashboard-title  { font-size:24px; font-weight:600; color:#2C3E50; flex-grow:1; }

      /*  ---- info box ---- */
      .description-box  { background:#f8f9fa; border-left:4px solid #217346;
                          padding:10px 15px; margin-bottom:20px; border-radius:0 4px 4px 0; }

      /*  ---- cart icon ---- */
      .cart-icon        { transition:transform .2s; }
      .cart-icon:hover  { transform:scale(1.2); }
    "))
  ),
  
  ## ── Header ───────────────────────────────────────────────────────────────────
  div(class="dashboard-header",
      tags$img(src="https://www.telquestintl.com/sca-dev-2022-2-0/img/telquest.jpg",
               class="dashboard-logo"),
      div(class="dashboard-title", icon("chart-bar"), "Real Time Inventory Dashboard")
  ),
  
  ## ── Main card ────────────────────────────────────────────────────────────────
  div(class="card",
      div(class="card-header",
          span(icon("database"), "Inventory Data"),
          downloadButton("download_csv", "Export as CSV",
                         class="btn-excel action-btn",
                         icon=icon("file-excel"))
      ),
      div(class="card-body",
          div(class="description-box",
              p(icon("info-circle"), " Welcome to the TelQuest Inventory Dashboard! Use the filters below each column heading to refine the data.")
          ),
          div(class="table-responsive",
              DTOutput("data_table")
          )
      )
  )
)

server <- function(input, output, session) {
  ## --- NetSuite OAuth details (unchanged) ------------------------------------
  NETSUITE_DEPLOYMENT_URL <- ''
  NETSUITE_REST_URL       <- ''
  NETSUITE_SCRIPT_ID      <- '3612'
  NETSUITE_DEPLOY_ID      <- '1'
  NETSUITE_ACCOUNT        <- ''
  NETSUITE_CONSUMER_KEY   <- ''
  NETSUITE_CONSUMER_SECRET<- ''
  NETSUITE_TOKEN_ID       <- ''
  NETSUITE_TOKEN_SECRET   <- ''
  
  ## --- SQL query (unchanged) --------------------------------------------------
  query <- "
    SELECT
      customrecord_bbl.custrecord_bbl_brokerbin_part_number,
      item.description,
      customlist_awa_brand.name              AS manufacturer,
      customlist_awa_condition.name          AS Condition,
      CASE 
        WHEN customrecord_bbl.custrecord_bbl_listed_brokerbin_quantity <= 250 
        THEN customrecord_bbl.custrecord_bbl_listed_brokerbin_quantity
        ELSE 250
      END                                     AS custrecord_bbl_listed_brokerbin_quantity,
      CONCAT('$', customrecord_bbl.custrecord_bbl_update_brokerbin_price),
      CONCAT('https://www.telquestintl.com/', item.urlcomponent) AS Link
    FROM 
      customrecord_bbl
    LEFT JOIN item                 ON customrecord_bbl.custrecord_bbl_item        = item.id
    LEFT JOIN customlist_awa_brand ON item.custitem_awa_brand                     = customlist_awa_brand.id
    LEFT JOIN customlist_awa_condition ON item.custitem_awa_condition             = customlist_awa_condition.id
    WHERE
      customrecord_bbl.custrecord_bbl_listed_brokerbin_quantity > 0
      AND customrecord_bbl.custrecord_bbl_main_listing = '1'
  "
  encoded <- toJSON(list(query=query), auto_unbox = TRUE)
  
  ## --- Helper: build OAuth header (unchanged) ---------------------------------
  oauth_nonce            <- 'ABCDEFGH'
  oauth_timestamp        <- as.character(as.integer(Sys.time()))
  oauth_signature_method <- 'HMAC-SHA256'
  oauth_version          <- '1.0'
  
  base_string <- paste0(
    "POST&", URLencode(NETSUITE_REST_URL, reserved = TRUE), "&",
    URLencode(paste0(
      "deploy=",NETSUITE_DEPLOY_ID,
      "&oauth_consumer_key=",NETSUITE_CONSUMER_KEY,
      "&oauth_nonce=",oauth_nonce,
      "&oauth_signature_method=",oauth_signature_method,
      "&oauth_timestamp=",oauth_timestamp,
      "&oauth_token=",NETSUITE_TOKEN_ID,
      "&oauth_version=",oauth_version,
      "&script=",NETSUITE_SCRIPT_ID
    ), reserved = TRUE)
  )
  hashkey         <- paste0(URLencode(NETSUITE_CONSUMER_SECRET, reserved=TRUE),"&",
                            URLencode(NETSUITE_TOKEN_SECRET, reserved=TRUE))
  oauth_signature <- base64_encode(hmac(hashkey, base_string, algo='sha256', raw=TRUE))
  
  auth_header <- paste0(
    'OAuth realm="',NETSUITE_ACCOUNT,'",',
    'oauth_consumer_key="',NETSUITE_CONSUMER_KEY,'",',
    'oauth_token="',NETSUITE_TOKEN_ID,'",',
    'oauth_signature_method="',oauth_signature_method,'",',
    'oauth_timestamp="',oauth_timestamp,'",',
    'oauth_nonce="',oauth_nonce,'",',
    'oauth_version="',oauth_version,'",',
    'oauth_signature="',URLencode(oauth_signature, reserved=TRUE),'"'
  )
  
  ## --- Fetch data reactively ---------------------------------------------------
  inventory_data <- reactive({
    withProgress(message="Fetching inventory data...", value=0,{
      res          <- POST(NETSUITE_DEPLOYMENT_URL,
                           body = encoded,
                           add_headers(.headers=c("Content-Type"="application/json",
                                                  "Authorization" = auth_header)))
      df           <- as.data.frame(fromJSON(content(res,"text")))
      colnames(df) <- c("Part Number","Description","Manufacturer","Condition",
                        "Quantity","Price","Link")
      cart_icon_url <- "https://www.telquestintl.com/site/images/Shop%20Cart%20Icon.png"
      df$Link      <- sprintf('<a href="%s" target="_blank">
                                 <img src="%s" alt="Cart" class="cart-icon" style="width:24px;height:24px;"></a>',
                              df$Link, cart_icon_url)
      df
    })
  })
  
  ## --- DataTable with per-column filters --------------------------------------
  output$data_table <- renderDataTable({
    DT::datatable(
      inventory_data(),
      rownames = FALSE,
      filter   = "top",                     # ← HERE: column filter boxes
      options  = list(
        dom        = '<"top"f>rt<"bottom"ip>',  # keep global filter + layout
        pageLength = 100,
        lengthMenu = list(c(50,100,150,250,-1), c('50','100','150','250','All')),
        searchDelay= 400,
        ordering   = TRUE,
        autoWidth  = TRUE,
        scrollX    = FALSE,
        columnDefs = list(
          list(targets="_all", className="dt-center"),
          list(targets=6, width="60px")   # Link column
        )
      ),
      class = "cell-border stripe",
      escape= FALSE
    ) %>% formatStyle(columns = 1:7, fontSize = '15px')
  }, server = FALSE)
  
  ## --- CSV download of filtered rows ------------------------------------------
  output$download_csv <- downloadHandler(
    filename = function() paste0("TelQuest_Inventory_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(input$data_table_rows_all)
      write.csv(inventory_data()[input$data_table_rows_all, 1:6],
                file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
