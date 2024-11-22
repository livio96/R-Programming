library(shiny)
library(httr)
library(jsonlite)
library(openssl)
library(digest)
library(DT)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Pricing Audit Dashboard"),
  mainPanel(
    width = 12,
    selectInput(
      "priority_filter",
      "Select Priority Level:",
      choices = c("All", "Priority 1", "Priority 2", "Priority 3"),
      selected = "All"
    ),
    downloadButton("download_csv", "Download Data as CSV"),  # Custom download button
    DTOutput("data_table")
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
  NETSUITE_ACCOUNT <- ''
  NETSUITE_CONSUMER_KEY <- ''
  NETSUITE_CONSUMER_SECRET <- ''
  NETSUITE_TOKEN_ID <- ''
  NETSUITE_TOKEN_SECRET <- ''
  
  # Corrected SQL Query
  query <- "
SELECT 
  it.itemid AS Item,
  SUM(crpss.custrecord_quantity_sold_7_days) AS \"Quantity Sold (All Orders - Last 7 Days)\",
  SUM(crpss.custrecord_quantity_sold_last_30_days) AS \"Quantity Sold (All Orders - Last 30 Days)\",
  MAX(it.custitem_bb_first_choice) AS \"Broker Bin First Choice Price\",
  MAX(it.custitem_end_user_price) AS \"Enduser Price\",
  MAX(it.custitem_set_ebay_price) AS \"Set eBay Price\",
  MAX(it.custitem40) AS \"Amazon FBA Price\",
  it.custitem_awa_brand AS \"WebStore Brand\"
FROM 
  customrecord_previous_sales_summary AS crpss
JOIN 
  item AS it
ON 
  crpss.custrecord_item_name = it.id
WHERE 
  it.isinactive = 'F' 
  AND crpss.custrecord_quantity_sold_last_30_days > 0
GROUP BY 
  it.itemid,
  it.custitem_awa_brand;
"
  
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
  
  # Make the POST request
  result <- POST(
    NETSUITE_DEPLOYMENT_URL,
    body = encoded,
    add_headers(.headers = c("Content-Type" = "application/json", "Authorization" = auth_header))
  )
  
  # Check for HTTP errors
  if (result$status_code != 200) {
    print(paste("Error: API request failed with status code", result$status_code))
    print(content(result, "text"))
  } else {
    # Convert results into a dataframe
    Output <- content(result, "text", encoding = "UTF-8")
    Output_json <- fromJSON(Output, flatten = TRUE)
    
    # Check if Output_json contains an error message
    if ("error" %in% names(Output_json)) {
      print(paste("API Error:", Output_json$error))
      output$data_table <- DT::renderDataTable({
        DT::datatable(data.frame(Message = Output_json$error))
      })
      # Disable the download button
      output$download_csv <- downloadHandler(
        filename = function() { NULL },
        content = function(file) { NULL }
      )
    } else {
      Output_df <- as.data.frame(Output_json)
      
      # Check if Output_df has data
      if (nrow(Output_df) == 0) {
        print("No data returned from API.")
        output$data_table <- DT::renderDataTable({
          DT::datatable(data.frame(Message = "No data available"))
        })
        # Disable the download button
        output$download_csv <- downloadHandler(
          filename = function() { NULL },
          content = function(file) { NULL }
        )
      } else {
        # Assign column names
        colnames(Output_df) <- c(
          "Part Number",
          "Sold-7-Days",
          "Sold-30-Days",
          "Brokerbin Price",
          "Business Price",
          "Ebay Price",
          "Amazon Price",
          "WebStore Brand"
        )[1:ncol(Output_df)]
        
        filtered_data <- reactive({
          req(Output_df)  # Ensure that Output_df is available
          priority <- input$priority_filter
          
          # Make sure price columns are numeric
          Output_df$`Business Price` <- as.numeric(Output_df$`Business Price`)
          Output_df$`Amazon Price` <- as.numeric(Output_df$`Amazon Price`)
          Output_df$`Ebay Price` <- as.numeric(Output_df$`Ebay Price`)
          
          df <- Output_df  # Copy Output_df to a new data frame
          
          if (priority == "Priority 1") {
            # Items where Amazon or Ebay Price is <= Business Price or between $1 and $6 greater
            df$Amazon_Diff <- df$`Amazon Price` - df$`Business Price`
            df$Ebay_Diff <- df$`Ebay Price` - df$`Business Price`
            
            # Handle NAs
            df$Amazon_Diff[is.na(df$Amazon_Diff)] <- Inf
            df$Ebay_Diff[is.na(df$Ebay_Diff)] <- Inf
            
            # Create logical conditions
            cond_amazon <- (df$Amazon_Diff <= 0) | (df$Amazon_Diff > 0 & df$Amazon_Diff <= 6)
            cond_ebay <- (df$Ebay_Diff <= 0) | (df$Ebay_Diff > 0 & df$Ebay_Diff <= 6)
            
            # Condition for Business Price > 50
            cond_business_price <- df$`Business Price` > 50
            
            # Combine conditions
            priority1_condition <- (cond_amazon | cond_ebay) & cond_business_price
            
            df_filtered <- df[priority1_condition, ]
            
          } else if (priority == "Priority 2") {
            # Items where Amazon or Ebay Price is $6-$12 greater than Business Price
            df$Amazon_Diff <- df$`Amazon Price` - df$`Business Price`
            df$Ebay_Diff <- df$`Ebay Price` - df$`Business Price`
            
            df$Amazon_Diff[is.na(df$Amazon_Diff)] <- -Inf
            df$Ebay_Diff[is.na(df$Ebay_Diff)] <- -Inf
            
            # Create logical conditions
            cond_amazon <- (df$Amazon_Diff > 6 & df$Amazon_Diff <= 12)
            cond_ebay <- (df$Ebay_Diff > 6 & df$Ebay_Diff <= 12)
            
            # Condition for Business Price > 50
            cond_business_price <- df$`Business Price` > 50
            
            priority2_condition <- (cond_amazon | cond_ebay) & cond_business_price
            
            df_filtered <- df[priority2_condition, ]
            
          } else if (priority == "Priority 3") {
            # Items where Amazon or Ebay Price is $12+ greater than Business Price
            df$Amazon_Diff <- df$`Amazon Price` - df$`Business Price`
            df$Ebay_Diff <- df$`Ebay Price` - df$`Business Price`
            
            df$Amazon_Diff[is.na(df$Amazon_Diff)] <- -Inf
            df$Ebay_Diff[is.na(df$Ebay_Diff)] <- -Inf
            
            # Create logical conditions
            cond_amazon <- df$Amazon_Diff > 12
            cond_ebay <- df$Ebay_Diff > 12
            
            priority3_condition <- cond_amazon | cond_ebay
            
            df_filtered <- df[priority3_condition, ]
            
          } else {
            # If "All" is selected, return all data
            df_filtered <- df
          }
          
          # Remove temporary columns
          df_filtered$Amazon_Diff <- NULL
          df_filtered$Ebay_Diff <- NULL
          
          df_filtered  # Return the filtered data frame
        })
        
        # Render the DataTable
        output$data_table <- DT::renderDataTable({
          DT::datatable(
            filtered_data(),
            extensions = c('Buttons', 'Scroller'),
            options = list(
              dom = 'frtip',  # Remove 'B' to hide built-in export buttons
              pageLength = 150,
              lengthMenu = list(c(50, 100, 150, -1), c('50', '100', '150', 'All')),
              searchDelay = 500,
              ordering = TRUE,
              columnDefs = list(
                list(targets = "_all", className = "dt-center")
              )
            ),
            filter = 'top'  # Position filter boxes above the table
          )
        }, server = FALSE)  # Set server = FALSE for client-side processing
        
        # Custom download handler for CSV
        output$download_csv <- downloadHandler(
          filename = function() {
            paste("TelQuest_Inventory_", Sys.Date(), ".csv", sep = "")
          },
          content = function(file) {
            # Wait for the DataTable to be rendered and inputs to be available
            req(input$data_table_rows_all)
            
            # Get the indices of the filtered data
            filtered_indices <- input$data_table_rows_all
            
            # Subset the data
            filtered_data_to_download <- filtered_data()[filtered_indices, ]
            
            # Write the filtered data to CSV
            write.csv(filtered_data_to_download, file, row.names = FALSE)
          }
        )
      }
    }
  }
}

# Run the application
shinyApp(ui = ui, server = server)
