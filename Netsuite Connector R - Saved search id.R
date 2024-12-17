library(shiny)
library(httr)
library(jsonlite)
library(openssl)
library(digest)
library(DT)

# Define UI for the app
ui <- fluidPage(
  titlePanel("TelQuest Real Time Inventory"),
  mainPanel(
    width = 12,
    downloadButton("download_csv", "Download Data as CSV"),  # Custom download button
    DTOutput("data_table")
  )
)

# Define server logic
server <- function(input, output, session) {
  # NetSuite Credentials and Endpoints
  NETSUITE_DEPLOYMENT_URL <- 'https://.restlets.api.netsuite.com/app/site/hosting/restlet.nl?script=3975&deploy=1'
  NETSUITE_ACCOUNT <- '586038'
  NETSUITE_CONSUMER_KEY <- ''
  NETSUITE_CONSUMER_SECRET <- ''
  NETSUITE_TOKEN_ID <- ''
  NETSUITE_TOKEN_SECRET <- ''
  
  # Script and Deployment IDs for the Restlet
  NETSUITE_REST_URL <- 'https://.restlets.api.netsuite.com/app/site/hosting/restlet.nl'
  NETSUITE_SCRIPT_ID <- '3975'
  NETSUITE_DEPLOY_ID <- '1'
  
  # Replace with the actual internal ID of your saved search
  SAVED_SEARCH_ID <- "customsearch_approved_bbl_listings_3_2"
  
  # Create the JSON payload to supply the saved search ID
  query_list <- list(searchId = SAVED_SEARCH_ID)
  encoded <- toJSON(query_list, auto_unbox = TRUE)
  
  # OAuth parameters
  oauth_nonce <- 'ABCDEFGH'  # In production, generate a unique nonce
  oauth_timestamp <- as.character(as.integer(Sys.time()))
  oauth_signature_method <- 'HMAC-SHA256'
  oauth_version <- "1.0"
  
  # Construct the base string for OAuth 1.0
  post_str <- "POST&"
  base_url_encoded <- URLencode(NETSUITE_REST_URL, reserved = TRUE)
  NETSUITE_DEPLOY_ID_STR <- paste("deploy=", NETSUITE_DEPLOY_ID, sep = "")
  NETSUITE_CONSUMER_KEY_STR <- paste("&oauth_consumer_key=", NETSUITE_CONSUMER_KEY, sep = "")
  oauth_nonce_str <- paste("&oauth_nonce=", oauth_nonce, sep = "")
  oauth_signature_method_str <- paste("&oauth_signature_method=", oauth_signature_method, sep = "")
  oauth_timestamp_str <- paste("&oauth_timestamp=", oauth_timestamp, sep = "")
  NETSUITE_TOKEN_ID_STR <- paste("&oauth_token=", NETSUITE_TOKEN_ID, sep = "")
  oauth_version_str <- paste("&oauth_version=", oauth_version, sep = "")
  NETSUITE_SCRIPT_ID_STR <- paste("&script=", NETSUITE_SCRIPT_ID, sep = "")
  
  concat_str1 <- paste(post_str, base_url_encoded, "&", sep = "")
  concat_str2 <- paste0(
    NETSUITE_DEPLOY_ID_STR,
    NETSUITE_CONSUMER_KEY_STR,
    oauth_nonce_str,
    oauth_signature_method_str,
    oauth_timestamp_str,
    NETSUITE_TOKEN_ID_STR,
    oauth_version_str,
    NETSUITE_SCRIPT_ID_STR
  )
  
  base_string <- paste(concat_str1, URLencode(concat_str2, reserved = TRUE), sep = "")
  
  # Create the signing key
  hashkey <- paste(URLencode(NETSUITE_CONSUMER_SECRET, reserved = TRUE), '&', URLencode(NETSUITE_TOKEN_SECRET, reserved = TRUE), sep = "")
  
  # Create the signature
  oauth_signature <- hmac(hashkey, object = base_string, algo = 'sha256', serialize = FALSE, raw = TRUE)
  oauth_signature <- base64_encode(oauth_signature)
  
  # Build the Authorization header
  auth_header <- paste0(
    'OAuth realm="', URLencode(NETSUITE_ACCOUNT, reserved = TRUE), '",',
    'oauth_consumer_key="', URLencode(NETSUITE_CONSUMER_KEY, reserved = TRUE), '",',
    'oauth_token="', URLencode(NETSUITE_TOKEN_ID, reserved = TRUE), '",',
    'oauth_signature_method="', URLencode(oauth_signature_method, reserved = TRUE), '",',
    'oauth_timestamp="', URLencode(oauth_timestamp, reserved = TRUE), '",',
    'oauth_nonce="', URLencode(oauth_nonce, reserved = TRUE), '",',
    'oauth_version="', URLencode(oauth_version, reserved = TRUE), '",',
    'oauth_signature="', URLencode(oauth_signature, reserved = TRUE), '"'
  )
  
  # Make the POST request
  result <- POST(
    NETSUITE_DEPLOYMENT_URL,
    body = encoded,
    add_headers(.headers = c("Content-Type" = "application/json", "Authorization" = auth_header))
  )
  
  # Process the returned JSON
  Output <- content(result, "text")
  Output_json <- fromJSON(Output)
  Output_df <- as.data.frame(Output_json)
  str(Output_df)
  head(Output_df)
  # Assign column names based on your saved search columns
  # Update these column names based on the actual columns from your saved search
  # Example: "Part Number", "Description", "Price", "Quantity", "Manufacturer", "Condition"
  colnames(Output_df) <- c("Part Number", "Description", "Price", "Quantity", "Manufacturer", "Condition")
  
  # Render the DataTable
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      Output_df,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'frtip',
        pageLength = 150,
        lengthMenu = list(c(50, 100, 150, -1), c('50', '100', '150', 'All')),
        searchDelay = 500,
        ordering = TRUE,
        columnDefs = list(list(targets = "_all", className = "dt-center"))
      ),
      filter = 'top',
      escape = FALSE
    )
  }, server = FALSE)
  
  # Custom download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("TelQuest_Inventory_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(input$data_table_rows_all)
      filtered_indices <- input$data_table_rows_all
      filtered_data <- Output_df[filtered_indices, ]
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
