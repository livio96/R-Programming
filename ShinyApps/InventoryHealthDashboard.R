# ─── Inventory Health Dashboard -  Enhanced UI/UX Version ─────────────────────
# ─────────────────────────────────────────────────────────────────────────────

# ── libraries ──
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(plotly)
library(shinyWidgets)

# ── data URL (spaces encoded as %20) ──
data_url <- ""

# ── helper: load + wrangle data ──
load_inventory <- function(url = data_url) {
  # Add cache-busting parameter to URL
  url_with_timestamp <- paste0(url, "?timestamp=", as.numeric(Sys.time()))
  
  raw <- read.csv(url(url_with_timestamp), stringsAsFactors = FALSE)
  
  raw %>% 
    mutate(
      across(everything(), ~trimws(.x)),
      Date_Received     = parse_date_time(Date.Received,
                                          orders = c("mdy HM", "mdy HMS", "mdy IMp", "ymd")),
      Age_Days          = as.numeric(Sys.Date() - as.Date(Date_Received)),
      Age_Months        = Age_Days / 30.44,
      Bin_Flag_CD       = str_detect(Bin.Number,  regex("^CD-", TRUE)),
      Bin_Flag_Special  = str_detect(Bin.Number,
                                     regex("^(RTV|Returns|Trash|On Hold|Incomplete|Amazon Removal)", TRUE)),
      Status_Good       = (Status %in% c("Good", "GOOD", "good")),
      Item_Inactive     = str_detect(Item, regex("inactive", TRUE)),
      # Convert numeric columns from character to numeric
      On.Hand         = as.numeric(gsub(",", "", On.Hand)),
      Available         = as.numeric(gsub(",", "", Available))
    ) %>%
    # Handle any NAs introduced by conversion
    mutate(
      On.Hand = ifelse(is.na(On.Hand), 0, On.Hand),
      Available = ifelse(is.na(Available), 0, Available)
    )
}

# ── UI ──
header <- dashboardHeader(
  title = "Inventory Health Monitor"
)

sidebar <- dashboardSidebar(
  tags$head(
    tags$style(HTML("
      .left-side, .main-sidebar {
        background-color: #2c3e50 !important;
      }
      .sidebar-menu > li > a {
        color: #ecf0f1 !important;
        border-left: 3px solid transparent;
        transition: all 0.3s ease;
      }
      .sidebar-menu > li:hover > a,
      .sidebar-menu > li.active > a {
        background-color: #34495e !important;
        border-left-color: #3498db !important;
      }
    "))
  ),
  
  sidebarMenu(
    id = "tabs",
    menuItem("Dashboard Overview", tabName = "overview", icon = icon("tachometer-alt")),
    menuItem("Critical Issues", icon = icon("exclamation-triangle"),
             menuSubItem("Negative Balances", tabName = "neg"),
             menuSubItem("Inactive Items", tabName = "inactive"),
             menuSubItem("Duplicate Serials", tabName = "dupes")
    ),
    menuItem("Aging Analysis", tabName = "aging", icon = icon("clock")),
    menuItem("Location Issues", icon = icon("map-marked-alt"),
             menuSubItem("CD-Bin Problems", tabName = "cd"),
             menuSubItem("Misplaced Items", tabName = "good")
    ),
    menuItem("Data Export", tabName = "export", icon = icon("download"))
  )
)

body <- dashboardBody(
  # Custom CSS for better styling
  tags$head(
    tags$style(HTML("
      /* Custom styles for enhanced UI */
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      
      body {
        font-family: 'Inter', sans-serif;
      }
      
      .content-wrapper, .right-side {
        background-color: #f7f9fc;
      }
      
      /* Enhanced header */
      .main-header .logo {
        font-weight: 600;
        font-size: 20px;
      }
      
      .main-header .navbar {
        background-color: #2c3e50 !important;
      }
      
      /* Enhanced value boxes */
      .small-box {
        border-radius: 15px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.07);
        background-image: linear-gradient(135deg, var(--box-color-light), var(--box-color-dark));
        transition: all 0.3s ease;
        border: none;
      }
      
      .small-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 15px rgba(0,0,0,0.1);
      }
      
      .small-box h3 {
        font-weight: 700;
        font-size: 2.5rem;
      }
      
      .small-box .icon {
        font-size: 80px;
        opacity: 0.3;
      }
      
      /* Enhanced boxes */
      .box {
        border-radius: 15px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.07);
        border: none;
        transition: all 0.3s ease;
      }
      
      .box:hover {
        box-shadow: 0 8px 15px rgba(0,0,0,0.1);
      }
      
      .box-header {
        border-bottom: 2px solid #f0f3f7;
        padding: 20px;
      }
      
      .box-title {
        font-weight: 600;
        font-size: 18px;
      }
      
      /* Enhanced info boxes */
      .info-box {
        border-radius: 12px;
        box-shadow: 0 3px 6px rgba(0,0,0,0.06);
        min-height: 110px;
        transition: all 0.3s ease;
        border: none;
      }
      
      .info-box:hover {
        transform: translateY(-3px);
        box-shadow: 0 6px 12px rgba(0,0,0,0.1);
      }
      
      .info-box-icon {
        border-radius: 12px 0 0 12px;
        width: 90px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      
      .info-box-number {
        font-size: 28px;
        font-weight: 700;
      }
      
      .info-box-text {
        font-weight: 500;
        text-transform: none;
      }
      
      /* Enhanced metric cards */
      .metric-card {
        background: white;
        border-radius: 15px;
        padding: 30px;
        margin: 10px 0;
        box-shadow: 0 4px 6px rgba(0,0,0,0.07);
        transition: all 0.3s ease;
      }
      
      .metric-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 15px rgba(0,0,0,0.1);
      }
      
      .metric-title {
        font-size: 14px;
        color: #8492a6;
        margin-bottom: 8px;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .metric-value {
        font-size: 36px;
        font-weight: 700;
        color: #1a202c;
        line-height: 1.2;
      }
      
      .metric-change {
        font-size: 14px;
        font-weight: 500;
        color: #10b981;
        margin-top: 8px;
      }
      
      .metric-change.negative {
        color: #ef4444;
      }
      
      /* Enhanced buttons */
      .btn {
        border-radius: 8px;
        font-weight: 500;
        padding: 10px 20px;
        transition: all 0.3s ease;
        border: none;
      }
      
      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }
      
      .action-button {
        background: white;
        border: 2px solid #e5e7eb;
        color: #374151;
        padding: 20px;
        text-align: center;
        border-radius: 12px;
        transition: all 0.3s ease;
        cursor: pointer;
      }
      
      .action-button:hover {
        border-color: #3b82f6;
        background: #f9fafb;
        transform: translateY(-3px);
      }
      
      .action-button h5 {
        margin: 10px 0;
        color: #1f2937;
        font-weight: 600;
      }
      
      .action-button .action-count {
        font-size: 32px;
        font-weight: 700;
        color: #3b82f6;
      }
      
      /* Enhanced datatables */
      .dataTables_wrapper {
        padding: 20px;
      }
      
      .dataTables_wrapper .dataTables_filter input {
        border-radius: 25px;
        padding: 8px 20px;
        border: 2px solid #e5e7eb;
        background: #f9fafb;
        font-size: 14px;
        transition: all 0.3s ease;
      }
      
      .dataTables_wrapper .dataTables_filter input:focus {
        border-color: #3b82f6;
        background: white;
        outline: none;
      }
      
      .dataTable th {
        background: #f9fafb;
        font-weight: 600;
        text-transform: uppercase;
        font-size: 12px;
        letter-spacing: 0.5px;
        color: #6b7280;
      }
      
      /* Value box gradient colors */
      .bg-blue {
        --box-color-light: #60a5fa;
        --box-color-dark: #3b82f6;
      }
      
      .bg-red {
        --box-color-light: #f87171;
        --box-color-dark: #ef4444;
      }
      
      .bg-yellow {
        --box-color-light: #fbbf24;
        --box-color-dark: #f59e0b;
      }
      
      .bg-green {
        --box-color-light: #34d399;
        --box-color-dark: #10b981;
      }
      
      /* Tabs */
      .nav-tabs-custom>.nav-tabs>li.active {
        border-top-color: #3b82f6;
      }
      
      .nav-tabs-custom>.nav-tabs>li.active>a {
        font-weight: 600;
      }
      
      /* Animations */
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .box, .info-box, .small-box, .metric-card {
        animation: fadeIn 0.5s ease-out;
      }
    "))
  ),
  
  # Load shinyWidgets for enhanced UI elements
  useSweetAlert(),
  
  tabItems(
    # ── Overview Dashboard ──
    tabItem("overview",
            fluidRow(
              column(12,
                     div(
                       style = "margin-bottom: 30px;",
                       h2("Inventory Health Overview", 
                          style = "font-weight: 700; color: #1a202c; margin-bottom: 10px;"),
                       uiOutput("lastUpdated")
                     )
              )
            ),
            
            # Key metrics summary
            fluidRow(
              valueBoxOutput("totalItems"),
              valueBoxOutput("itemsWithIssues"),
              valueBoxOutput("avgAge"),
              valueBoxOutput("totalOnHand")
            ),
            
            # Issue summary cards
            fluidRow(
              column(6,
                     box(
                       title = tagList(icon("chart-bar"), "Critical Issues Summary"),
                       status = "danger",
                       width = NULL,
                       solidHeader = FALSE,
                       plotlyOutput("issuesSummaryChart", height = "350px")
                     )
              ),
              column(6,
                     box(
                       title = tagList(icon("chart-pie"), "Aging Distribution"),
                       status = "warning",
                       width = NULL,
                       solidHeader = FALSE,
                       plotlyOutput("agingDistChart", height = "350px")
                     )
              )
            ),
            
            # Quick action cards
            fluidRow(
              column(12,
                     h3("Quick Actions", 
                        style = "font-weight: 600; color: #1a202c; margin: 30px 0 20px 0;"),
                     fluidRow(
                       column(3,
                              actionButton("viewNegatives",
                                           div(
                                             icon("minus-circle", class = "fa-3x", style = "color: #ef4444;"),
                                             h5("Negative Balances"),
                                             div(class = "action-count", textOutput("negCount", inline = TRUE))
                                           ),
                                           class = "action-button",
                                           style = "width: 100%; height: 140px; white-space: normal;"
                              )
                       ),
                       column(3,
                              actionButton("viewDupes",
                                           div(
                                             icon("clone", class = "fa-3x", style = "color: #f59e0b;"),
                                             h5("Duplicate Serials"),
                                             div(class = "action-count", textOutput("dupeCount", inline = TRUE))
                                           ),
                                           class = "action-button",
                                           style = "width: 100%; height: 140px; white-space: normal;"
                              )
                       ),
                       column(3,
                              actionButton("viewAging",
                                           div(
                                             icon("clock", class = "fa-3x", style = "color: #3b82f6;"),
                                             h5("Items > 6 Months"),
                                             div(class = "action-count", textOutput("agingCount", inline = TRUE))
                                           ),
                                           class = "action-button",
                                           style = "width: 100%; height: 140px; white-space: normal;"
                              )
                       ),
                       column(3,
                              actionButton("viewLocation",
                                           div(
                                             icon("map-marked-alt", class = "fa-3x", style = "color: #8b5cf6;"),
                                             h5("Location Issues"),
                                             div(class = "action-count", textOutput("locationCount", inline = TRUE))
                                           ),
                                           class = "action-button",
                                           style = "width: 100%; height: 140px; white-space: normal;"
                              )
                       )
                     )
              )
            )
    ),
    
    # ── Negative Balances ──
    tabItem("neg",
            fluidRow(
              column(12,
                     h2("Negative Balance Analysis", style = "font-weight: 700; color: #1a202c;"),
                     p("Items showing negative on-hand or available quantities", 
                       style = "color: #6b7280; margin-bottom: 30px; font-size: 16px;")
              )
            ),
            fluidRow(
              infoBoxOutput("negativeCount"),
              infoBoxOutput("negativeSKUs"),
              infoBoxOutput("negativeVariance")
            ),
            fluidRow(
              box(
                title = tagList(icon("table"), "Negative Balance Details"),
                width = 12,
                status = "danger",
                solidHeader = FALSE,
                DTOutput("tbl_neg")
              )
            )
    ),
    
    # ── Inactive Items ──
    tabItem("inactive",
            fluidRow(
              column(12,
                     h2("Inactive Items Analysis", style = "font-weight: 700; color: #1a202c;"),
                     p("Items marked as inactive but still showing inventory", 
                       style = "color: #6b7280; margin-bottom: 30px; font-size: 16px;")
              )
            ),
            fluidRow(
              infoBoxOutput("inactiveCount"),
              infoBoxOutput("inactiveOnHand"),
              infoBoxOutput("inactiveAge")
            ),
            fluidRow(
              box(
                title = tagList(icon("table"), "Inactive Items Details"),
                width = 12,
                status = "warning",
                solidHeader = FALSE,
                DTOutput("tbl_inactive")
              )
            )
    ),
    
    # ── Duplicate Serials ──
    tabItem("dupes",
            fluidRow(
              column(12,
                     h2("Duplicate Serial Analysis", style = "font-weight: 700; color: #1a202c;"),
                     p("Serial numbers appearing multiple times in inventory", 
                       style = "color: #6b7280; margin-bottom: 30px; font-size: 16px;")
              )
            ),
            fluidRow(
              infoBoxOutput("dupesUnique"),
              infoBoxOutput("dupesTotal"),
              infoBoxOutput("dupesLocations")
            ),
            fluidRow(
              box(
                title = tagList(icon("table"), "Duplicate Serial Details"),
                width = 12,
                status = "info",
                solidHeader = FALSE,
                DTOutput("tbl_dupes")
              )
            )
    ),
    
    # ── Aging Analysis ──
    tabItem("aging",
            fluidRow(
              column(12,
                     h2("Inventory Aging Analysis", style = "font-weight: 700; color: #1a202c;"),
                     p("Distribution of inventory by age categories", 
                       style = "color: #6b7280; margin-bottom: 30px; font-size: 16px;")
              )
            ),
            fluidRow(
              infoBoxOutput("age90"),
              infoBoxOutput("age180"),
              infoBoxOutput("age365")
            ),
            fluidRow(
              infoBoxOutput("age540"),
              infoBoxOutput("age720"),
              box(
                title = "Aging Summary",
                status = "primary",
                width = 4,
                div(
                  style = "text-align: center; padding: 20px;",
                  icon("chart-pie", class = "fa-3x", style = "color: #3b82f6; margin-bottom: 10px;"),
                  p("View detailed aging breakdown", style = "color: #6b7280;")
                )
              )
            ),
            
            # Aging visualization
            fluidRow(
              box(
                title = tagList(icon("chart-bar"), "Aging Distribution Chart"),
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                plotlyOutput("agingChart", height = "400px")
              )
            ),
            
            # Tabbed aging details
            fluidRow(
              box(
                title = tagList(icon("table"), "Aging Details by Period"),
                width = 12,
                tabsetPanel(
                  id = "agingTabs",
                  tabPanel("90+ Days", icon = icon("calendar-day"), DTOutput("tbl_a90")),
                  tabPanel("6+ Months", icon = icon("calendar-alt"), DTOutput("tbl_a180")),
                  tabPanel("12+ Months", icon = icon("calendar"), DTOutput("tbl_a365")),
                  tabPanel("18+ Months", icon = icon("calendar-plus"), DTOutput("tbl_a540")),
                  tabPanel("24+ Months", icon = icon("calendar-check"), DTOutput("tbl_a720"))
                )
              )
            )
    ),
    
    # ── CD-Bin Issues ──
    tabItem("cd",
            fluidRow(
              column(12,
                     h2("CD-Bin Status Issues", style = "font-weight: 700; color: #1a202c;"),
                     p("Items in CD bins without 'Good' status", 
                       style = "color: #6b7280; margin-bottom: 30px; font-size: 16px;")
              )
            ),
            fluidRow(
              infoBoxOutput("cdCount"),
              infoBoxOutput("cdBins"),
              infoBoxOutput("cdStatuses")
            ),
            fluidRow(
              box(
                title = tagList(icon("table"), "CD-Bin Issue Details"),
                width = 12,
                status = "info",
                solidHeader = FALSE,
                DTOutput("tbl_cd")
              )
            )
    ),
    
    # ── Misplaced Good Items ──
    tabItem("good",
            fluidRow(
              column(12,
                     h2("Misplaced Good Items", style = "font-weight: 700; color: #1a202c;"),
                     p("Items with 'Good' status in special/problem bins", 
                       style = "color: #6b7280; margin-bottom: 30px; font-size: 16px;")
              )
            ),
            fluidRow(
              infoBoxOutput("misplacedCount"),
              infoBoxOutput("misplacedBins"),
              infoBoxOutput("misplacedQuantity")
            ),
            fluidRow(
              box(
                title = tagList(icon("table"), "Misplaced Items Details"),
                width = 12,
                status = "warning",
                solidHeader = FALSE,
                DTOutput("tbl_misplaced")
              )
            )
    ),
    
    # ── Data Export ──
    tabItem("export",
            fluidRow(
              column(12,
                     h2("Export Data", style = "font-weight: 700; color: #1a202c;"),
                     p("Download inventory data in various formats", 
                       style = "color: #6b7280; margin-bottom: 30px; font-size: 16px;")
              )
            ),
            fluidRow(
              column(6,
                     box(
                       title = tagList(icon("file-export"), "Export Options"),
                       width = NULL,
                       status = "success",
                       solidHeader = FALSE,
                       h4("Select Data to Export:", style = "font-weight: 600; margin-bottom: 20px;"),
                       prettyCheckboxGroup(
                         "exportOptions",
                         "",
                         choices = list(
                           "Negative Balances" = "negatives",
                           "Inactive Items" = "inactive",
                           "Duplicate Serials" = "dupes",
                           "Aging Analysis" = "aging",
                           "CD-Bin Issues" = "cd",
                           "Misplaced Items" = "good",
                           "Complete Inventory" = "all"
                         ),
                         selected = "all",
                         icon = icon("check"),
                         animation = "smooth"
                       ),
                       br(),
                       downloadBttn(
                         "downloadData",
                         "Download Selected Data",
                         style = "gradient",
                         color = "success",
                         size = "lg",
                         block = TRUE
                       )
                     )
              ),
              column(6,
                     box(
                       title = tagList(icon("info-circle"), "Export Summary"),
                       width = NULL,
                       status = "info",
                       solidHeader = FALSE,
                       h4("Available Reports:", style = "font-weight: 600; margin-bottom: 20px;"),
                       uiOutput("exportSummary")
                     )
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# ── server ──
server <- function(input, output, session) {
  
  # Reactive value for last update time
  lastUpdateTime <- reactiveVal(Sys.time())
  
  # Main reactive data loading
  inventory_data <- reactive({
    # Trigger on refresh button
    input$refreshBtn
    
    # Also auto-refresh every 5 minutes (300000 milliseconds)
    invalidateLater(300000, session)
    
    # Update last refresh time
    lastUpdateTime(Sys.time())
    
    # Load data with error handling
    tryCatch({
      withProgress(message = 'Loading inventory data...', value = 0, {
        setProgress(0.5)
        data <- load_inventory()
        setProgress(1)
        data
      })
    }, error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error Loading Data",
        text = "Using previous data if available.",
        type = "error"
      )
      NULL
    })
  })
  
  # Create reactive subsets
  negatives <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(On.Hand < 0 | Available < 0)
  })
  
  inactive_items <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Item_Inactive & (On.Hand != 0 | Available != 0))
  })
  
  dupes <- reactive({
    req(inventory_data())
    inventory_data() %>% 
      filter(!is.na(Inventory.Number) & 
               Inventory.Number != "" & 
               trimws(Inventory.Number) != "" &
               nchar(trimws(Inventory.Number)) > 3) %>%
      group_by(Inventory.Number) %>% 
      filter(n() > 1) %>% 
      ungroup()
  })
  
  age_90 <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Age_Days > 90)
  })
  
  age_180 <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Age_Days > 180)
  })
  
  age_365 <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Age_Days > 365)
  })
  
  age_540 <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Age_Days > 540)
  })
  
  age_720 <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Age_Days > 720)
  })
  
  cd_wrong_status <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Bin_Flag_CD & !Status_Good)
  })
  
  misplaced_good <- reactive({
    req(inventory_data())
    inventory_data() %>% filter(Bin_Flag_Special & Status_Good)
  })
  
  # Last update display
  output$lastUpdated <- renderUI({
    tags$p(
      style = "color: #6b7280; font-size: 14px; margin-top: 5px;",
      icon("clock"),
      " Last updated: ",
      format(lastUpdateTime(), "%B %d, %Y at %I:%M %p")
    )
  })
  
  # Update all value boxes to be reactive
  output$totalItems <- renderValueBox({
    valueBox(
      value = format(nrow(inventory_data()), big.mark = ","),
      subtitle = "Total Inventory Items",
      icon = icon("boxes"),
      color = "blue"
    )
  })
  
  output$itemsWithIssues <- renderValueBox({
    req(inventory_data())
    issues_count <- sum(c(nrow(negatives()), nrow(inactive_items()), 
                          nrow(dupes()), nrow(cd_wrong_status()), 
                          nrow(misplaced_good())))
    valueBox(
      value = paste0(round(issues_count / nrow(inventory_data()) * 100, 1), "%"),
      subtitle = "Items with Issues",
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  output$avgAge <- renderValueBox({
    valueBox(
      value = paste0(round(mean(inventory_data()$Age_Days, na.rm = TRUE)), " days"),
      subtitle = "Average Age",
      icon = icon("calendar-alt"),
      color = "yellow"
    )
  })
  
  output$totalOnHand <- renderValueBox({
    valueBox(
      value = format(sum(inventory_data()$On.Hand, na.rm = TRUE), big.mark = ","),
      subtitle = "Total On-Hand Quantity",
      icon = icon("warehouse"),
      color = "green"
    )
  })
  
  # Quick action outputs
  output$negCount <- renderText(nrow(negatives()))
  output$dupeCount <- renderText(length(unique(dupes()$Inventory.Number)))
  output$agingCount <- renderText(nrow(age_180()))
  output$locationCount <- renderText(nrow(cd_wrong_status()) + nrow(misplaced_good()))
  
  # Update all info boxes to be reactive
  output$negativeCount <- renderInfoBox({
    infoBox("Critical Items", 
            format(nrow(negatives()), big.mark = ","), 
            icon = icon("minus-circle"), 
            color = "red")
  })
  
  output$negativeSKUs <- renderInfoBox({
    infoBox("Affected SKUs", 
            format(length(unique(negatives()$Item)), big.mark = ","), 
            icon = icon("barcode"), 
            color = "orange")
  })
  
  output$negativeVariance <- renderInfoBox({
    infoBox("Total Variance", 
            format(sum(negatives()$On.Hand[negatives()$On.Hand < 0]), big.mark = ","), 
            icon = icon("calculator"), 
            color = "purple")
  })
  
  # Similar updates for all other info boxes...
  output$inactiveCount <- renderInfoBox({
    infoBox("Inactive Items", 
            format(nrow(inactive_items()), big.mark = ","), 
            icon = icon("ban"), 
            color = "yellow")
  })
  
  output$inactiveOnHand <- renderInfoBox({
    infoBox("Quantity on Hand", 
            format(sum(inactive_items()$On.Hand, na.rm = TRUE), big.mark = ","), 
            icon = icon("cubes"), 
            color = "orange")
  })
  
  output$inactiveAge <- renderInfoBox({
    infoBox("Average Age", 
            paste0(round(mean(inactive_items()$Age_Days, na.rm = TRUE)), " days"), 
            icon = icon("clock"), 
            color = "maroon")
  })
  
  output$dupesUnique <- renderInfoBox({
    infoBox("Unique Duplicates", 
            format(length(unique(dupes()$Inventory.Number)), big.mark = ","), 
            icon = icon("fingerprint"), 
            color = "aqua")
  })
  
  output$dupesTotal <- renderInfoBox({
    infoBox("Total Entries", 
            format(nrow(dupes()), big.mark = ","), 
            icon = icon("copy"), 
            color = "blue")
  })
  
  output$dupesLocations <- renderInfoBox({
    infoBox("Unique Locations", 
            format(length(unique(dupes()$Bin.Number)), big.mark = ","), 
            icon = icon("map-marker-alt"), 
            color = "teal")
  })
  
  output$age90 <- renderInfoBox({
    infoBox("90+ Days", 
            format(nrow(age_90()), big.mark = ","), 
            icon = icon("history"), 
            color = "green",
            fill = TRUE)
  })
  
  output$age180 <- renderInfoBox({
    infoBox("6+ Months", 
            format(nrow(age_180()), big.mark = ","), 
            icon = icon("clock"), 
            color = "yellow",
            fill = TRUE)
  })
  
  output$age365 <- renderInfoBox({
    infoBox("12+ Months", 
            format(nrow(age_365()), big.mark = ","), 
            icon = icon("calendar"), 
            color = "orange",
            fill = TRUE)
  })
  
  output$age540 <- renderInfoBox({
    infoBox("18+ Months", 
            format(nrow(age_540()), big.mark = ","), 
            icon = icon("calendar-check"), 
            color = "red",
            fill = TRUE)
  })
  
  output$age720 <- renderInfoBox({
    infoBox("24+ Months", 
            format(nrow(age_720()), big.mark = ","), 
            icon = icon("calendar-times"), 
            color = "maroon",
            fill = TRUE)
  })
  
  output$cdCount <- renderInfoBox({
    infoBox("Items Affected", 
            format(nrow(cd_wrong_status()), big.mark = ","), 
            icon = icon("compact-disc"), 
            color = "purple")
  })
  
  output$cdBins <- renderInfoBox({
    infoBox("CD Bins", 
            format(length(unique(cd_wrong_status()$Bin.Number)), big.mark = ","), 
            icon = icon("box"), 
            color = "blue")
  })
  
  output$cdStatuses <- renderInfoBox({
    infoBox("Unique Statuses", 
            length(unique(cd_wrong_status()$Status)), 
            icon = icon("tags"), 
            color = "navy")
  })
  
  output$misplacedCount <- renderInfoBox({
    infoBox("Misplaced Items", 
            format(nrow(misplaced_good()), big.mark = ","), 
            icon = icon("exclamation"), 
            color = "yellow")
  })
  
  output$misplacedBins <- renderInfoBox({
    infoBox("Problem Bins", 
            format(length(unique(misplaced_good()$Bin.Number)), big.mark = ","), 
            icon = icon("archive"), 
            color = "orange")
  })
  
  output$misplacedQuantity <- renderInfoBox({
    infoBox("Total Quantity", 
            format(sum(misplaced_good()$On.Hand, na.rm = TRUE), big.mark = ","), 
            icon = icon("boxes"), 
            color = "red")
  })
  
  # Update charts to use reactive data with enhanced styling
  output$issuesSummaryChart <- renderPlotly({
    req(inventory_data())
    issues_data <- data.frame(
      Category = c("Negative Balances", "Inactive Items", "Duplicate Serials", 
                   "CD-Bin Issues", "Misplaced Items"),
      Count = c(nrow(negatives()), nrow(inactive_items()), 
                length(unique(dupes()$Inventory.Number)),
                nrow(cd_wrong_status()), nrow(misplaced_good()))
    )
    
    plot_ly(issues_data, x = ~Category, y = ~Count, type = 'bar',
            marker = list(color = c('#ef4444', '#f59e0b', '#3b82f6', '#8b5cf6', '#f97316'),
                          line = list(color = 'white', width = 2))) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Number of Items", gridcolor = '#f3f4f6'),
             showlegend = FALSE,
             plot_bgcolor = 'rgba(0,0,0,0)',
             paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$agingDistChart <- renderPlotly({
    req(inventory_data())
    inv <- inventory_data()
    aging_data <- data.frame(
      Period = c("0-90 days", "90-180 days", "180-365 days", "365-540 days", "540-720 days", "720+ days"),
      Count = c(
        nrow(inv %>% filter(Age_Days <= 90)),
        nrow(inv %>% filter(Age_Days > 90 & Age_Days <= 180)),
        nrow(inv %>% filter(Age_Days > 180 & Age_Days <= 365)),
        nrow(inv %>% filter(Age_Days > 365 & Age_Days <= 540)),
        nrow(inv %>% filter(Age_Days > 540 & Age_Days <= 720)),
        nrow(inv %>% filter(Age_Days > 720))
      )
    )
    
    plot_ly(aging_data, labels = ~Period, values = ~Count, type = 'pie',
            marker = list(colors = c('#10b981', '#6b7280', '#f59e0b', 
                                     '#f97316', '#ef4444', '#991b1b'),
                          line = list(color = 'white', width = 2))) %>%
      layout(showlegend = TRUE,
             plot_bgcolor = 'rgba(0,0,0,0)',
             paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$agingChart <- renderPlotly({
    req(inventory_data())
    inv <- inventory_data()
    
    aging_categories <- data.frame(
      Category = c("0-90 days", "90-180 days", "180-365 days", 
                   "365-540 days", "540-720 days", "720+ days"),
      Count = c(
        nrow(inv %>% filter(Age_Days <= 90)),
        nrow(inv %>% filter(Age_Days > 90 & Age_Days <= 180)),
        nrow(inv %>% filter(Age_Days > 180 & Age_Days <= 365)),
        nrow(inv %>% filter(Age_Days > 365 & Age_Days <= 540)),
        nrow(inv %>% filter(Age_Days > 540 & Age_Days <= 720)),
        nrow(inv %>% filter(Age_Days > 720))
      )
    )
    
    aging_categories$Category <- factor(aging_categories$Category, 
                                        levels = aging_categories$Category)
    
    plot_ly(aging_categories, x = ~Category, y = ~Count, type = 'bar',
            marker = list(
              color = c('#10b981', '#6b7280', '#f59e0b', '#f97316', '#ef4444', '#991b1b'),
              line = list(color = 'white', width = 2)
            )) %>%
      layout(
        xaxis = list(title = "Age Category", tickangle = -45),
        yaxis = list(title = "Number of Items", gridcolor = '#f3f4f6'),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Enhanced table rendering
  render_inv_table <- function(data) {
    datatable(
      data,
      extensions   = c("Buttons", "Responsive", "Scroller"),
      rownames     = FALSE,
      filter       = "top",
      options      = list(
        dom       = "Bfrtip",
        buttons   = list(
          list(extend = "csv", text = "CSV", className = "btn btn-sm btn-export"),
          list(extend = "excel", text = "Excel", className = "btn btn-sm btn-export"),
          list(extend = "pdf", text = "PDF", className = "btn btn-sm btn-export")
        ),
        pageLength = 25,
        scrollX    = TRUE,
        scrollY    = "400px",
        scroller   = TRUE,
        responsive = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({'background-color': '#f9fafb', 'color': '#6b7280'});",
          "  $(this.api().table().container()).find('.dt-buttons').css('margin-bottom', '10px');",
          "}"
        )
      )
    )
  }
  
  # Update table outputs to use reactive data
  output$tbl_neg       <- renderDT(render_inv_table(negatives()))
  output$tbl_inactive  <- renderDT(render_inv_table(inactive_items()))
  output$tbl_dupes     <- renderDT(render_inv_table(dupes()))
  output$tbl_a90       <- renderDT(render_inv_table(age_90()))
  output$tbl_a180      <- renderDT(render_inv_table(age_180()))
  output$tbl_a365      <- renderDT(render_inv_table(age_365()))
  output$tbl_a540      <- renderDT(render_inv_table(age_540()))
  output$tbl_a720      <- renderDT(render_inv_table(age_720()))
  output$tbl_cd        <- renderDT(render_inv_table(cd_wrong_status()))
  output$tbl_misplaced <- renderDT(render_inv_table(misplaced_good()))
  
  # Navigation from overview buttons
  observeEvent(input$refreshBtn, {
    sendSweetAlert(
      session = session,
      title = "Refreshing Data",
      text = "Loading latest inventory information...",
      type = "info",
      timer = 1500,
      showConfirmButton = FALSE
    )
  })
  
  # Add click handlers for action buttons using standard observeEvent
  observeEvent(input$viewNegatives, {
    updateTabItems(session, "tabs", selected = "neg")
  })
  
  observeEvent(input$viewDupes, {
    updateTabItems(session, "tabs", selected = "dupes")
  })
  
  observeEvent(input$viewAging, {
    updateTabItems(session, "tabs", selected = "aging")
  })
  
  observeEvent(input$viewLocation, {
    updateTabItems(session, "tabs", selected = "cd")
  })
  
  # Update export summary to be reactive
  output$exportSummary <- renderUI({
    req(inventory_data())
    tags$div(
      style = "padding: 10px;",
      tags$ul(
        style = "list-style: none; padding-left: 0;",
        tags$li(
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "Negative Balances: ", strong(format(nrow(negatives()), big.mark = ",")), " items"
        ),
        tags$li(
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "Inactive Items: ", strong(format(nrow(inactive_items()), big.mark = ",")), " items"
        ),
        tags$li(
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "Duplicate Serials: ", strong(format(length(unique(dupes()$Inventory.Number)), big.mark = ",")), " unique serials"
        ),
        tags$li(
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "Aging > 90 days: ", strong(format(nrow(age_90()), big.mark = ",")), " items"
        ),
        tags$li(
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "CD-Bin Issues: ", strong(format(nrow(cd_wrong_status()), big.mark = ",")), " items"
        ),
        tags$li(
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "Misplaced Good: ", strong(format(nrow(misplaced_good()), big.mark = ",")), " items"
        ),
        tags$li(
          icon("check-circle", style = "color: #10b981; margin-right: 8px;"),
          "Total Inventory: ", strong(format(nrow(inventory_data()), big.mark = ",")), " items"
        )
      )
    )
  })
  
  # Download handler with reactive data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("inventory_export_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      datasets <- list(
        negatives = negatives(),
        inactive = inactive_items(),
        dupes = dupes(),
        aging = age_90(),
        cd = cd_wrong_status(),
        good = misplaced_good(),
        all = inventory_data()
      )
      
      selected_data <- data.frame()
      for (option in input$exportOptions) {
        if (option %in% names(datasets)) {
          selected_data <- rbind(selected_data, datasets[[option]])
        }
      }
      
      write.csv(selected_data, file, row.names = FALSE)
      
      sendSweetAlert(
        session = session,
        title = "Export Complete!",
        text = "Your data has been successfully exported.",
        type = "success",
        timer = 2000,
        showConfirmButton = FALSE
      )
    }
  )
}

# ── run app ──
shinyApp(ui, server)
