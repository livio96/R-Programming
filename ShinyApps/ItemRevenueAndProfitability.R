library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(shinydashboard)
library(shinycssloaders)

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Data source URL (spaces encoded)
data_url <- "https://www.telquestintl.com/Item%20Revenue%20and%20Profitability%20Dashboard/.csv"

# Updated value box function with enhanced styling
define_valueBox <- function(value, title, icon_name, color, trend = NULL, trend_icon = NULL, trend_color = NULL) {
  # Define color schemes based on the color parameter
  color_schemes <- list(
    "bg-blue" = list(bg = "#3498db", icon_bg = "#2980b9", text = "#ffffff"),
    "bg-green" = list(bg = "#2ecc71", icon_bg = "#27ae60", text = "#ffffff"),
    "bg-orange" = list(bg = "#f39c12", icon_bg = "#d35400", text = "#ffffff"),
    "bg-red" = list(bg = "#e74c3c", icon_bg = "#c0392b", text = "#ffffff"),
    "bg-purple" = list(bg = "#9b59b6", icon_bg = "#8e44ad", text = "#ffffff")
  )
  
  # Get color scheme or default to blue if not found
  scheme <- color_schemes[[color]] %||% color_schemes[["bg-blue"]]
  
  # Trend HTML if provided
  trend_html <- ""
  if (!is.null(trend)) {
    trend_html <- sprintf(
      '<div class="trend %s"><i class="fa %s"></i> %s</div>',
      trend_color %||% "neutral",
      trend_icon %||% "fa-arrow-right",
      trend
    )
  }
  
  # Create the HTML for the value box
  HTML(sprintf(
    '<div class="modern-value-box" style="background-color: %s; color: %s;">
      <div class="value-icon" style="background-color: %s;">
        <i class="fa fa-%s fa-2x"></i>
      </div>
      <div class="value-content">
        <div class="value-title">%s</div>
        <div class="value-number">%s</div>
        %s
      </div>
    </div>',
    scheme$bg, scheme$text, scheme$icon_bg, icon_name, title, value, trend_html
  ))
}

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly", primary = "#217346", secondary = "#217346", base_font = font_google("Roboto")),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML(
      "body { font-family:'Roboto','Segoe UI',sans-serif; font-size:14px; color:#212529; background:#f6f8fa; }
       .dashboard-header { display:flex; align-items:center; padding:15px 0; margin-bottom:25px; background:#fff; border-bottom:2px solid #e7e7e7; box-shadow:0 2px 5px rgba(0,0,0,0.05); }
       .dashboard-logo { height:45px; margin-right:20px; }
       .dashboard-title { font-size:24px; font-weight:600; color:#2C3E50; flex-grow:1; }
       .btn-filter { background:#5bc0de; color:white; border:none; }
       .btn-reset { background:#f0ad4e; color:white; margin-left:10px; }
       .card { border-radius:8px; box-shadow:0 3px 10px rgba(0,0,0,0.08); background:#fff; margin-bottom:24px; border:1px solid #e7e7e7; }
       .card-header { padding:15px; border-bottom:1px solid #e7e7e7; font-weight:600; }
       .plot-container { height:350px; }
      "
    )),
    tags$style(HTML('
      .modern-value-box {
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        display: flex;
        margin-bottom: 24px;
        overflow: hidden;
        transition: transform 0.2s, box-shadow 0.2s;
        height: 120px;
      }
      
      .modern-value-box:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 15px rgba(0,0,0,0.15);
      }
      
      .value-icon {
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 0 20px;
        width: 80px;
      }
      
      .value-content {
        flex: 1;
        padding: 15px 20px;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      
      .value-title {
        font-size: 16px;
        opacity: 0.9;
        margin-bottom: 5px;
        font-weight: 400;
      }
      
      .value-number {
        font-size: 28px;
        font-weight: 700;
        margin-bottom: 5px;
        letter-spacing: -0.5px;
      }
      
      .trend {
        font-size: 14px;
        display: flex;
        align-items: center;
        gap: 5px;
      }
      
      .trend.up { color: #2ecc71; }
      .trend.down { color: #e74c3c; }
      .trend.neutral { opacity: 0.7; }
      
      .trend i {
        font-size: 12px;
      }
    '))
  ),
  
  # Dashboard Header
  div(class = "dashboard-header",
      tags$img(src = "https://www.telquestintl.com/sca-dev-2022-2-0/img/telquest.jpg", class = "dashboard-logo"),
      div(class = "dashboard-title", icon("chart-line", class = "fa-lg"), "Item Revenue and Profitability"),
      div(style = "text-align: right;", span(icon("clock"), "Last Updated: ", textOutput("last_updated", inline = TRUE)))
  ),
  
  # Main Tabs
  fluidRow(
    column(12,
           tabsetPanel(id = "main_tabs",
                       
                       # Dashboard Tab
                       tabPanel("Dashboard",
                                br(),
                                fluidRow(
                                  column(4, uiOutput("summary_revenue")),
                                  column(4, uiOutput("summary_profit")),
                                  column(4, uiOutput("summary_gp_pct"))
                                ),
                                
                                # Filters
                                div(class = "card",
                                    div(class = "card-header",
                                        span(icon("filter"), "Filters"),
                                        div(actionButton("apply_filters", "Apply Filters", icon = icon("search"), class = "btn-filter"),
                                            actionButton("reset_filters", "Reset", icon = icon("undo"), class = "btn-reset"))
                                    ),
                                    div(class = "card-body",
                                        fluidRow(
                                          column(4, selectInput("channel", "Sales Channel", choices = NULL)),
                                          column(4, selectInput("category", "Category", choices = NULL)),
                                          column(4, selectInput("manufacturer", "Manufacturer", choices = NULL))
                                        )
                                    )
                                ),
                                
                                # Charts
                                fluidRow(
                                  column(6,
                                         div(class = "card",
                                             div(class = "card-header", span(icon("chart-bar"), "Top 10 Profitable Items")),
                                             div(class = "card-body", withSpinner(plotlyOutput("plot_top")))
                                         )
                                  ),
                                  column(6,
                                         div(class = "card",
                                             div(class = "card-header", span(icon("chart-bar"), "Bottom 10 Profitable Items")),
                                             div(class = "card-body", withSpinner(plotlyOutput("plot_bottom")))
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         div(class = "card",
                                             div(class = "card-header", span(icon("chart-pie"), "Sales by Category")),
                                             div(class = "card-body", withSpinner(plotlyOutput("plot_category")))
                                         )
                                  ),
                                  column(6,
                                         div(class = "card",
                                             div(class = "card-header", span(icon("building"), "Sales by Manufacturer")),
                                             div(class = "card-body", withSpinner(plotlyOutput("plot_manufacturer")))
                                         )
                                  )
                                )
                       ),
                       
                       # Inventory Data Tab
                       tabPanel("Inventory Data",
                                br(),
                                div(class = "card",
                                    div(class = "card-header",
                                        span(icon("database"), "Inventory Data"),
                                        downloadButton("download_csv", "Export as CSV", class = "btn-excel", icon = icon("file-excel"))
                                    ),
                                    div(class = "card-body", withSpinner(DTOutput("data_table")))
                                )
                       ),
                       
                       # Analysis Tab
                       tabPanel("Analysis",
                                br(),
                                div(class = "card",
                                    div(class = "card-header", span(icon("chart-line"), "Profitability Trends")),
                                    div(class = "card-body", withSpinner(plotlyOutput("plot_profitability_trend", height = "400px")))
                                ),
                                div(class = "card",
                                    div(class = "card-header", span(icon("lightbulb"), "Key Insights")),
                                    div(class = "card-body", uiOutput("insights"))
                                )
                       )
                       
           )
    )
  ),
  
  # Footer
  tags$footer(
    div(style = "text-align: center; padding: 20px; margin-top: 30px; border-top: 1px solid #e7e7e7; color: #777;",
        "© 2025 Telquest International, Inc. All rights reserved.")
  )
)

server <- function(input, output, session) {
  # Load and prepare data
  df <- reactive({
    withProgress(message = 'Loading data...', value = 0.5, {
      read.csv(data_url, stringsAsFactors = FALSE, check.names = FALSE) %>%
        mutate(
          `Quantity Sold` = as.numeric(`Quantity Sold`),     # <<< convert to numeric
          Revenue         = as.numeric(Revenue),
          Cost            = as.numeric(Cost),
          GP              = as.numeric(GP),
          `GP %`          = as.character(`GP %`),
          GP.Pct          = as.numeric(gsub("%", "", `GP %`)),
          Channel         = `eTail Channel`,
          Category        = Category,
          Manufacturer    = Manufacturer
        ) %>%
        filter(!is.na(`Quantity Sold`) & `Quantity Sold` > 0) %>%
        mutate(Date = sample(seq(as.Date('2025-01-01'), as.Date('2025-05-01'), by = "day"), n(), replace = TRUE))
    })
  })
  
  # Initialize filters
  observe({
    data <- df()
    updateSelectInput(session, "channel",      choices = c("All", sort(unique(data$Channel))),      selected = "All")
    updateSelectInput(session, "category",     choices = c("All", sort(unique(data$Category))),     selected = "All")
    updateSelectInput(session, "manufacturer", choices = c("All", sort(unique(data$Manufacturer))), selected = "All")
  })
  
  # Reactive filtered data
  filters <- reactiveValues(data = NULL)
  observeEvent(input$apply_filters, {
    dat <- df()
    if (input$channel      != "All") dat <- filter(dat, Channel      == input$channel)
    if (input$category     != "All") dat <- filter(dat, Category     == input$category)
    if (input$manufacturer != "All") dat <- filter(dat, Manufacturer == input$manufacturer)
    filters$data <- dat
  })
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "channel",      selected = "All")
    updateSelectInput(session, "category",     selected = "All")
    updateSelectInput(session, "manufacturer", selected = "All")
    filters$data <- df()
  })
  observe({ if (is.null(filters$data)) filters$data <- df() })
  filtered <- reactive(filters$data)
  
  # Last updated
  output$last_updated <- renderText({ format(Sys.time(), "%B %d, %Y %H:%M:%S") })
  
  # Improved summary boxes with trends
  output$summary_revenue <- renderUI({
    current_revenue <- sum(filtered()$Revenue, na.rm = TRUE)
    prev_period <- df() %>% 
      filter(!is.na(Date)) %>%
      mutate(Period = ifelse(Date >= Sys.Date() - 30, "Current", "Previous")) %>%
      group_by(Period) %>%
      summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop")
    
    trend_pct <- NULL
    trend_icon <- NULL
    trend_color <- NULL
    
    
    
    define_valueBox(
      dollar(current_revenue),
      "Total Revenue", 
      "dollar-sign", 
      "bg-blue",
      trend_pct,
      trend_icon,
      trend_color
    )
  })
  
  output$summary_profit <- renderUI({
    current_profit <- sum(filtered()$GP, na.rm = TRUE)
    prev_period <- df() %>% 
      filter(!is.na(Date)) %>%
      mutate(Period = ifelse(Date >= Sys.Date() - 30, "Current", "Previous")) %>%
      group_by(Period) %>%
      summarise(GP = sum(GP, na.rm = TRUE), .groups = "drop")
    
    trend_pct <- NULL
    trend_icon <- NULL
    trend_color <- NULL
    
   
    
    define_valueBox(
      dollar(current_profit),
      "Total Gross Profit", 
      "chart-line", 
      "bg-green",
      trend_pct,
      trend_icon,
      trend_color
    )
  })
  
  output$summary_gp_pct <- renderUI({
    current_gp_pct <- mean(filtered()$GP.Pct, na.rm = TRUE)
    prev_period <- df() %>% 
      filter(!is.na(Date)) %>%
      mutate(Period = ifelse(Date >= Sys.Date() - 30, "Current", "Previous")) %>%
      group_by(Period) %>%
      summarise(GP_Pct = mean(GP.Pct, na.rm = TRUE), .groups = "drop")
    
    trend_pct <- NULL
    trend_icon <- NULL
    trend_color <- NULL
    
   
    
    define_valueBox(
      paste0(round(current_gp_pct, 1), "%"),
      "Average GP %", 
      "percentage", 
      "bg-orange",
      trend_pct,
      trend_icon,
      trend_color
    )
  })
  
  # Data table (grouped by Item)
  output$data_table <- renderDT({
    dat <- filtered() %>%
      group_by(Item, Description, Manufacturer) %>%
      summarise(
        `Quantity Sold` = sum(`Quantity Sold`, na.rm = TRUE),
        Revenue         = sum(Revenue, na.rm = TRUE),
        Cost            = sum(Cost, na.rm = TRUE),
        GP              = sum(GP, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        `GP %` = ifelse(Revenue > 0,
                        paste0(round(GP / Revenue * 100, 1), "%"),
                        NA_character_)
      ) %>%
      arrange(desc(Revenue))
    
    datatable(dat, rownames = FALSE, extensions = c('Buttons','Responsive'), options = list(
      dom = 'Blfrtip',
      buttons = list(
        list(extend='copy',  className='btn-sm'),
        list(extend='csv',   className='btn-sm'),
        list(extend='excel', className='btn-sm')
      ),
      pageLength = 15,
      scrollX    = TRUE,
      columnDefs = list(
        list(width = '250px', targets = 1),
        list(className = 'dt-center', targets = c(3,7))
      )
    ), class = 'cell-border stripe hover') %>%
      formatCurrency(c('Revenue','Cost','GP'), '$') %>%
      formatStyle(
        'GP',
        color           = styleInterval(0, c('red','black')),
        backgroundColor = styleInterval(0, c('#ffeeee','#eeffee'))
      )
  })
  
  # CSV export
  output$download_csv <- downloadHandler(
    filename = function() paste0('telquest-inventory-', format(Sys.time(), '%Y-%m-%d'), '.csv'),
    content  = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  
  # Top 10 chart
  output$plot_top <- renderPlotly({
    top10 <- filtered() %>% top_n(10, GP) %>% arrange(desc(GP))
    p <- ggplot(top10, aes(x = reorder(Item, GP), y = GP,
                           text = paste0("Item: ", Item,
                                         "<br>Description: ", Description,
                                         "<br>Revenue: ", dollar(Revenue),
                                         "<br>Gross Profit: ", dollar(GP),
                                         "<br>GP %: ", `GP %`))) +
      geom_col(fill='#00a65a', width=0.7) + coord_flip() +
      labs(x='', y='Gross Profit') +
      theme_minimal() +
      theme(axis.text.y=element_text(size=11),
            axis.title.x=element_text(size=12,face='bold'))
    ggplotly(p, tooltip='text')
  })
  
  # Bottom 10 chart
  output$plot_bottom <- renderPlotly({
    bot10 <- filtered() %>% top_n(-10, GP) %>% arrange(GP)
    p <- ggplot(bot10, aes(x = reorder(Item, -GP), y = GP,
                           text = paste0("Item: ", Item,
                                         "<br>Description: ", Description,
                                         "<br>Revenue: ", dollar(Revenue),
                                         "<br>Gross Profit: ", dollar(GP),
                                         "<br>GP %: ", `GP %`))) +
      geom_col(fill=ifelse(bot10$GP < 0, '#d9534f', '#00a65a'), width=0.7) +
      coord_flip() +
      labs(x='', y='Gross Profit') +
      theme_minimal() +
      theme(axis.text.y=element_text(size=11),
            axis.title.x=element_text(size=12,face='bold'))
    ggplotly(p, tooltip='text')
  })
  
  # Category pie
  output$plot_category <- renderPlotly({
    by_cat <- filtered() %>%
      group_by(Category) %>%
      summarize(
        Revenue = sum(Revenue, na.rm=TRUE),
        GP      = sum(GP, na.rm=TRUE),
        GP_Pct  = mean(GP.Pct, na.rm=TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Revenue))
    
    if(nrow(by_cat) > 8) {
      top8  <- by_cat[1:8,]
      other <- data.frame(
        Category = 'Other',
        Revenue  = sum(by_cat$Revenue[9:nrow(by_cat)]),
        GP       = sum(by_cat$GP[9:nrow(by_cat)]),
        GP_Pct   = mean(by_cat$GP_Pct[9:nrow(by_cat)])
      )
      by_cat <- bind_rows(top8, other)
    }
    
    plot_ly(by_cat,
            labels   = ~Category,
            values   = ~Revenue,
            type     = 'pie',
            hole     = 0.4,
            textinfo = 'label+percent',
            hoverinfo= 'text',
            text     = ~paste0(Category, '<br>Revenue: ', dollar(Revenue)))
  })
  
  # Manufacturer bar
  output$plot_manufacturer <- renderPlotly({
    by_man <- filtered() %>%
      group_by(Manufacturer) %>%
      summarize(Revenue = sum(Revenue, na.rm=TRUE), .groups = "drop") %>%
      arrange(desc(Revenue)) %>%
      slice_head(n = 10)
    
    plot_ly(by_man,
            x         = ~Manufacturer,
            y         = ~Revenue,
            type      = 'bar',
            hoverinfo = 'text',
            text      = ~paste0(Manufacturer, '<br>Revenue: ', dollar(Revenue)))
  })
  
  # Profitability trends
  output$plot_profitability_trend <- renderPlotly({
    trend <- filtered() %>%
      group_by(Date) %>%
      summarize(Revenue = sum(Revenue, na.rm=TRUE),
                GP      = sum(GP, na.rm=TRUE),
                .groups = "drop") %>%
      arrange(Date)
    
    plot_ly(trend, x=~Date, y=~Revenue, name='Revenue', type='scatter', mode='lines') %>%
      add_trace(y=~GP, name='Gross Profit')
  })
  
  # Insights
  output$insights <- renderUI({
    # Get the raw data
    raw_data <- filtered()
    
    # Create grouped data by Item to get accurate counts
    grouped_data <- raw_data %>%
      group_by(Item, Description, Manufacturer) %>%
      summarise(
        Revenue = sum(Revenue, na.rm = TRUE),
        GP = sum(GP, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate metrics based on the grouped data
    total     <- nrow(grouped_data)
    prof      <- sum(grouped_data$GP > 0, na.rm=TRUE)
    unprof    <- sum(grouped_data$GP <= 0, na.rm=TRUE)
    best      <- grouped_data[which.max(grouped_data$GP), ]
    worst     <- grouped_data[which.min(grouped_data$GP), ]
    
    # Manufacturer stats still from raw data to get accurate revenue/profit sums
    manu_top  <- raw_data %>%
      group_by(Manufacturer) %>%
      summarize(TGP = sum(GP, na.rm=TRUE), .groups = "drop") %>%
      arrange(desc(TGP)) %>%
      slice_head(n = 1)
    
    tagList(
      tags$h4("Performance Summary"),
      tags$ul(
        tags$li(strong("Total Items:"), total),
        tags$li(strong("Profitable Items:"), prof),
        tags$li(strong("Unprofitable Items:"), unprof),
        tags$li(strong("Top Manufacturer:"), manu_top$Manufacturer)
      ),
      tags$h4("Notable Products"),
      tags$ul(
        tags$li(strong("Most Profitable:"), best$Item),
        tags$li(strong("Least Profitable:"), worst$Item)
      )
    )
  })
}

# Run the app
shinyApp(ui, server)
