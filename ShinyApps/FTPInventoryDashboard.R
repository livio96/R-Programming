# app.R

# Load required libraries
library(shiny)
library(RCurl)           # For FTP connection
library(readr)           # For reading delimited files
library(DT)              # For displaying data tables
library(dplyr)           # For data manipulation
library(shinycssloaders) # For loading spinner

# Hardcoded FTP credentials and file details
ftp_username <- ""
ftp_password <- ""
ftp_host     <- "ftp.dandh.com"
ftp_file     <- "/itemlist"  # Adjust the path and filename accordingly

# Define the UI
ui <- fluidPage(
  titlePanel("FTP Data Dashboard"),
  
  # Full-width and full-height table
  fluidRow(
    column(
      width = 12,
      # Loading spinner while data is being fetched
      withSpinner(
        DTOutput("dataTable"),
        type = 6  # Spinner type
      )
    )
  ),
  
  # Adjust table height to fill the viewport
  tags$style(HTML("
    .dataTables_wrapper .dataTables_scrollBody {
      max-height: calc(100vh - 150px) !important;
    }
  "))
)

# Define the server logic
server <- function(input, output, session) {
  
  # Function to download and process data from FTP
  getData <- function() {
    # Construct the FTP URL
    ftp_url <- paste0("ftp://", ftp_username, ":", ftp_password, "@", ftp_host, ftp_file)
    
    # Use getURL to download the file content as a string
    file_data <- getURL(ftp_url)
    
    # Read the data from the string using read_delim with '|' as delimiter
    data <- read_delim(
      file = I(file_data),
      delim = '|',
      col_names = FALSE,
      na = c("", "NA"),
      trim_ws = TRUE,
      quote = "",
      escape_double = FALSE,
      locale = locale(encoding = "UTF-8"),
      skip_empty_rows = TRUE
    )
    
    # Ensure that the data has enough columns
    if (ncol(data) < 18) {
      showNotification("Data does not have enough columns. Please check the data file.", type = "error")
      return(NULL)
    }
    
    # Keep specified columns and rename them
    data <- data %>%
      select(
        quantity     = X2,
        SKU          = X5,  # Updated from X4 to X5
        MPN          = X6,  # Updated from X5 to X6
        manufacturer = X9,
        cost         = X10,
        description  = X16,
        MSRP         = X18
      )
    
    return(data)
  }
  
  # Load data and render the data table
  output$dataTable <- renderDT({
    data <- getData()
    if (is.null(data)) {
      return(NULL)
    }
    datatable(
      data,
      options = list(
        pageLength = 100,    # Default to showing 100 results
        autoWidth = TRUE,
        scrollY = 'calc(100vh - 150px)',  # Adjust table height
        scrollX = TRUE,
        scroller = TRUE,
        deferRender = TRUE,
        # Enable filtering on specified columns
        columnDefs = list(
          list(targets = c(4, 6), searchable = FALSE)  # Disable search on 'cost' and 'MSRP'
        )
      ),
      filter = 'top'  # Enable filters at the top of the table
    )
  }, server = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)
