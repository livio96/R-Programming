library(shiny)
library(httr)
library(jsonlite)
library(openssl)
library(digest)
library(DT)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Export Netsuite Data For Opporunities"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("search_items", "Search Items (line separated):", rows = 10, placeholder = "Enter item numbers, each on a new line."),
      actionButton("search_button", "Search")
    ),
    mainPanel(
      width = 12,  # Ensure the main panel takes up all available width
      DTOutput("data_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Tokens and NetSuite query
  NETSUITE_DEPLOYMENT_URL <- 'https://.restlets.api.netsuite.com/app/site/hosting/restlet.nl?script=3612&deploy=1'
  NETSUITE_URL <- 'https://.restlets.api.netsuite.com'
  NETSUITE_REST_URL <- 'https://.restlets.api.netsuite.com/app/site/hosting/restlet.nl'
  NETSUITE_SCRIPT_ID <- '3612'
  NETSUITE_DEPLOY_ID <- '1'
  NETSUITE_ACCOUNT <- '586038'
  NETSUITE_CONSUMER_KEY <- ''
  NETSUITE_CONSUMER_SECRET <- ''
  NETSUITE_TOKEN_ID <- ''
  NETSUITE_TOKEN_SECRET <- ''
  
  observeEvent(input$search_button, {
    search_items <- strsplit(input$search_items, "\n")[[1]]
    search_items <- trimws(search_items)
    search_items <- search_items[search_items != ""]
    
    if (length(search_items) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please enter at least one item to search.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    # Create the dynamic WHERE clause for the SQL query using LIKE for contains and lower for case-insensitive
    where_clauses <- paste0("LOWER(customrecord_bbl.custrecord_bbl_brokerbin_part_number) LIKE LOWER('%", search_items, "%')")
    where_clause <- paste(where_clauses, collapse = " OR ")
    
    query <- sprintf("
    SELECT
      customrecord_bbl.custrecord_bbl_brokerbin_part_number,
      customrecord_bbl.custrecord_bbl_update_brokerbin_price,
      item.custitem_end_user_price AS BusinessPrice,
      CASE 
        WHEN customrecord_bbl.custrecord_bbl_listed_brokerbin_quantity <= 250 THEN customrecord_bbl.custrecord_bbl_listed_brokerbin_quantity
        ELSE 250
      END AS custrecord_bbl_listed_brokerbin_quantity,
      item.description,
      customlist_awa_brand.name AS manufacturer,
      customlist_awa_condition.name as Condition,
      item.custitem_set_ebay_price AS EbayPrice,
      item.custitem40 AS AmazonFBAPrice,
      item.custitem_lowest_buybox AS LowestBuyBox,
      item.averagecost AS AverageCost,
      item.custitem_quantity_sold_last_7_days as QtySold7Days,
      item.custitem_quantity_sold_last_30_days as QtySold30Days,
      item.custitem_quantity_sold_last_90_days as QtySold90Days
    FROM 
      customrecord_bbl
    LEFT JOIN 
      item ON customrecord_bbl.custrecord_bbl_item = item.id
    LEFT JOIN 
      customlist_awa_brand ON item.custitem_awa_brand = customlist_awa_brand.id
    LEFT JOIN 
      customlist_awa_condition ON item.custitem_awa_condition = customlist_awa_condition.id
    WHERE
      %s
    ", where_clause)
    
    # Create a list with the query
    query_list <- list(query = query)
    
    # Convert the list to JSON
    encoded <- toJSON(query_list, auto_unbox = TRUE)
    
    oauth_nonce <- 'ABCDEFGH'
    oauth_timestamp <- as.character(as.integer(Sys.time()))
    oauth_signature_method <- 'HMAC-SHA256'
    oauth_version <- "1.0"
    
    post_str <- base_string <- paste("POST", "&", sep = "")
    
    NETSUITE_REST_URL_STR <- paste(URLencode(NETSUITE_REST_URL, reserved = TRUE), "&", sep = "")
    
    NETSUITE_DEPLOY_ID_STR <- paste("deploy=", NETSUITE_DEPLOY_ID, sep = "")
    NETSUITE_CONSUMER_KEY_STR <- paste("&oauth_consumer_key=", NETSUITE_CONSUMER_KEY, sep = "")
    oauth_nonce_str <- paste("&oauth_nonce=", oauth_nonce, sep = "")
    oauth_signature_method_str <- paste("&oauth_signature_method=", oauth_signature_method, sep = "")
    oauth_timestamp_str <- paste("&oauth_timestamp=", oauth_timestamp, sep = "")
    NETSUITE_TOKEN_ID_STR <- paste("&oauth_token=", NETSUITE_TOKEN_ID, sep = "")
    oauth_version_str <- paste("&oauth_version=", oauth_version, sep = "")
    NETSUITE_SCRIPT_ID_STR <- paste("&script=", NETSUITE_SCRIPT_ID, sep = "")
    
    concat_str1 <- paste(post_str, NETSUITE_REST_URL_STR, sep = "")
    concat_str2 <- paste(NETSUITE_DEPLOY_ID_STR, NETSUITE_CONSUMER_KEY_STR, sep = "")
    concat_str2 <- paste(concat_str2, oauth_nonce_str, sep = "")
    concat_str2 <- paste(concat_str2, oauth_signature_method_str, sep = "")
    concat_str2 <- paste(concat_str2, oauth_timestamp_str, sep = "")
    concat_str2 <- paste(concat_str2, NETSUITE_TOKEN_ID_STR, sep = "")
    concat_str2 <- paste(concat_str2, oauth_version_str, sep = "")
    concat_str2 <- paste(concat_str2, NETSUITE_SCRIPT_ID_STR, sep = "")
    
    base_string <- paste(concat_str1, URLencode(concat_str2, reserved = TRUE), sep = "")
    hashkey <- paste(URLencode(NETSUITE_CONSUMER_SECRET, reserved = TRUE), '&', sep = "")
    hashkey <- paste(hashkey, URLencode(NETSUITE_TOKEN_SECRET, reserved = TRUE), sep = "")
    
    oauth_signature <- hmac(hashkey, object = base_string, algo = 'sha256', serialize = FALSE, raw = TRUE)
    oauth_signature <- base64_encode(oauth_signature)
    
    auth_header <- 'OAuth '
    auth_header <- paste(auth_header, 'realm="', sep = "")
    auth_header <- paste(auth_header, URLencode(NETSUITE_ACCOUNT, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '",', sep = "")
    
    auth_header <- paste(auth_header, 'oauth_consumer_key="', sep = "")
    auth_header <- paste(auth_header, URLencode(NETSUITE_CONSUMER_KEY, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '",', sep = "")
    
    auth_header <- paste(auth_header, 'oauth_token="', sep = "")
    auth_header <- paste(auth_header, URLencode(NETSUITE_TOKEN_ID, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '",', sep = "")
    
    auth_header <- paste(auth_header, 'oauth_signature_method="', sep = "")
    auth_header <- paste(auth_header, URLencode(oauth_signature_method, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '",', sep = "")
    
    auth_header <- paste(auth_header, 'oauth_timestamp="', sep = "")
    auth_header <- paste(auth_header, URLencode(oauth_timestamp, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '",', sep = "")
    
    auth_header <- paste(auth_header, 'oauth_nonce="', sep = "")
    auth_header <- paste(auth_header, URLencode(oauth_nonce, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '",', sep = "")
    
    auth_header <- paste(auth_header, 'oauth_version="', sep = "")
    auth_header <- paste(auth_header, URLencode(oauth_version, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '",', sep = "")
    
    auth_header <- paste(auth_header, 'oauth_signature="', sep = "")
    auth_header <- paste(auth_header, URLencode(oauth_signature, reserved = TRUE), sep = "")
    auth_header <- paste(auth_header, '"', sep = "")
    
    result <- POST(NETSUITE_DEPLOYMENT_URL,
                   body = encoded,
                   add_headers(.headers = c("Content-Type" = "application/json", "Authorization" = auth_header)))
    
    # Convert results into a dataframe 
    Output <- content(result, "text")
    Output_json <- fromJSON(Output)
    Output_df <- as.data.frame(Output_json)
    colnames(Output_df) <- c("Part Number", "First Choice Price", "Business Price", "Quantity", "Description", "Manufacturer", "Condition", "Ebay Price", "Amazon FBA Price", "Lowest BuyBox", "Average Cost", "QtySold7Days","QtySold30Days", "QtySold90Days")
    
    # Render DataTable with formatted Link column
    output$data_table <- DT::renderDataTable({
      DT::datatable(Output_df,
                    extensions = c('Buttons', 'Scroller'),
                    options = list(
                      dom = 'Bfrtip',
                      buttons = list('copy', 'csv', 'excel', 'pdf', 'print'),
                      pageLength = 150,
                      lengthMenu = list(c(50, 100, 150, -1), c('50', '100', '150', 'All')),
                      searchDelay = 500,
                      ordering = TRUE,
                      columnDefs = list(
                        list(targets = "_all", className = "dt-center")
                      )
                    ),
                    filter = 'top',  # Position filter boxes above the table
                    escape = FALSE   # Allow HTML rendering
      )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
