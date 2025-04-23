# app.R – Inventory Aging Dashboard with Excel-Style Table and Last Purchaser Filter + Price Review Exclusion

library(shiny)
library(httr)
library(jsonlite)
library(openssl)
library(digest)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(DT)

ui <- fluidPage(
  tags$head(tags$style(HTML(
    "
    body, label, input, button, select, table {
      font-family: 'Segoe UI', Helvetica, Arial, sans-serif;
      font-size: 1.25rem;
    }

    .realtime-tag {
      position: absolute;
      top: 12px;
      right: 20px;
      background-color: #28a745;
      color: white;
      font-size: 0.85rem;
      font-weight: 600;
      padding: 6px 14px;
      border-radius: 20px;
      box-shadow: 0 0 0 rgba(40, 167, 69, 0.4);
      animation: pulse 2s infinite;
      z-index: 1000;
    }

    @keyframes pulse {
      0% { box-shadow: 0 0 0 0 rgba(40, 167, 69, 0.4); }
      70% { box-shadow: 0 0 0 10px rgba(40, 167, 69, 0); }
      100% { box-shadow: 0 0 0 0 rgba(40, 167, 69, 0); }
    }

    table.dataTable {
      border: 1px solid #dee2e6 !important;
      border-collapse: collapse !important;
      width: 100% !important;
    }

    table.dataTable thead th, table.dataTable tbody td {
      padding: 8px 12px !important;
      border: 1px solid #dee2e6 !important;
      text-align: left;
      vertical-align: middle;
    }

    table.dataTable tbody tr {
      height: 28px;
    }

    table.dataTable tfoot th {
      font-weight: bold;
      background-color: #f8f9fa;
      border-top: 2px solid #dee2e6 !important;
    }

    td.numeric {
      font-family: 'Courier New', Courier, monospace;
      text-align: right;
    }
    "
  ))),
  
  div(class = "realtime-tag", "Real‑time Connection to NetSuite"),
  
  titlePanel("Inventory Aging & Movement Dashboard (Live)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("manufacturer", "Manufacturer(s):", choices = NULL, multiple = TRUE),
      selectInput("last_purchaser", "Last Purchaser:", choices = NULL, selected = "All", multiple = FALSE),
      checkboxGroupInput("reasons", "Show items with reason(s):", choiceNames = NULL, choiceValues = NULL),
      checkboxInput("recent", "Exclude items received in last 15 days", FALSE),
      checkboxInput("exclude_recent_price_review", "Exclude items with price review in last 30 days", FALSE),
      downloadButton("downloadData", "Export CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 plotOutput("reasonBar", height = 280),
                 plotOutput("monthsHist", height = 280),
                 plotOutput("manufacturerHeatmap", height = 400)
        ),
        tabPanel("Details",
                 DTOutput("inventoryTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  make_choice_labels <- function(levels, colours) {
    lapply(seq_along(levels), function(i) {
      lbl <- levels[i]
      col <- colours[lbl]
      shiny::HTML(sprintf(
        '<span style="background-color:%s;width:14px;height:14px;display:inline-block;border-radius:2px;margin-right:6px;"></span>%s',
        col, lbl
      ))
    })
  }
  
  inventoryRaw <- reactive({
    withProgress(message = 'Loading data from NetSuite...', value = 0.1, {
      NETSUITE_REST_URL <- 'https://586038.restlets.api.netsuite.com/app/site/hosting/restlet.nl'
      acct    <- '586038'; script  <- '3975'; deploy <- '1'
      restlet_url <- paste0(NETSUITE_REST_URL, '?script=', script, '&deploy=', deploy)
      SAVED_SEARCH_ID <- 'customsearch382490'
      
      CONSUMER_KEY    <- ''
      CONSUMER_SECRET <- ''
      TOKEN_ID        <- ''
      TOKEN_SECRET    <- ''
      
      body_json       <- toJSON(list(searchId = SAVED_SEARCH_ID), auto_unbox = TRUE)
      oauth_nonce     <- paste0(sample(c(letters, LETTERS, 0:9), 16, TRUE), collapse = "")
      oauth_timestamp <- as.character(as.integer(Sys.time()))
      oauth_sig_method<- 'HMAC-SHA256'; oauth_version <- "1.0"
      param_str <- paste0(
        "deploy=", deploy,
        "&oauth_consumer_key=", CONSUMER_KEY,
        "&oauth_nonce=", oauth_nonce,
        "&oauth_signature_method=", oauth_sig_method,
        "&oauth_timestamp=", oauth_timestamp,
        "&oauth_token=", TOKEN_ID,
        "&oauth_version=", oauth_version,
        "&script=", script
      )
      base_string <- paste0(
        "POST&",
        URLencode(NETSUITE_REST_URL, TRUE), "&",
        URLencode(param_str, TRUE)
      )
      signing_key <- paste0(URLencode(CONSUMER_SECRET, TRUE), "&", URLencode(TOKEN_SECRET, TRUE))
      raw_sig <- hmac(signing_key, base_string, algo = "sha256", serialize = FALSE, raw = TRUE)
      oauth_sig <- base64_encode(raw_sig)
      auth_header <- paste0(
        'OAuth realm="', acct, '",',
        'oauth_consumer_key="', CONSUMER_KEY, '",',
        'oauth_token="', TOKEN_ID, '",',
        'oauth_signature_method="', oauth_sig_method, '",',
        'oauth_timestamp="', oauth_timestamp, '",',
        'oauth_nonce="', oauth_nonce, '",',
        'oauth_version="', oauth_version, '",',
        'oauth_signature="', URLencode(oauth_sig, TRUE), '"'
      )
      res <- POST(
        url = restlet_url,
        body = body_json,
        encode = "raw",
        add_headers('Content-Type' = 'application/json','Authorization' = auth_header)
      )
      if (http_error(res)) stop(sprintf('RESTlet request failed [%s]: %s', status_code(res), content(res,'text')))
      parsed <- fromJSON(content(res,'text'), flatten = TRUE)
      if (any(names(parsed)==""|is.na(names(parsed)))) {
        idx <- which(names(parsed)==""|is.na(names(parsed)))
        names(parsed)[idx] <- paste0("unknown_column_", idx)
      }
      df <- as_tibble(parsed) %>%
        rename(
          Item.Name                 = itemid,
          Description               = salesdescription,
          `On.Hand`                 = locationquantityonhand,
          `On Order`                = quantityonorder,
          Value                     = formulanumeric,
          Manufacturer              = formulatext,
          `Quantity.Sold.in.30.days` = custrecord_quantity_sold_last_30_days,
          `Quantity.Sold.in.90.days` = custrecord_quantity_sold_last_90_days,
          Last.Price.Review         = custitem_last_price_review,
          Last.Receipt.Date         = custitem_last_receipt_date,
          Last.Purchaser            = custitem_last_purchaser
        ) %>%
        select(Item.Name, Description, `On.Hand`, `On Order`, Value,
               Manufacturer, `Quantity.Sold.in.30.days`, `Quantity.Sold.in.90.days`,
               Last.Price.Review, Last.Receipt.Date, Last.Purchaser) %>%
        mutate(across(c(`On.Hand`,`On Order`,Value,`Quantity.Sold.in.30.days`,`Quantity.Sold.in.90.days`), as.numeric),
               Last.Price.Review = coalesce(as.Date(Last.Price.Review, "%m/%d/%Y"), parse_date_time(Last.Price.Review, c("mdy","dmy"))),
               Last.Receipt.Date = coalesce(as.Date(Last.Receipt.Date, "%Y-%m-%d"), parse_date_time(Last.Receipt.Date, c("mdy","dmy"))))
      if (!nrow(df)) { showNotification("No data returned", type='warning'); return(tibble()) }
      df
    })
  })
  
  inventory <- reactive({ req(inventoryRaw()); inv <- inventoryRaw();
  inv %>% mutate(
    QtySold30    = replace_na(`Quantity.Sold.in.30.days`, 0),
    QtySold90    = replace_na(`Quantity.Sold.in.90.days`, 0),
    MonthsOnHand = case_when(QtySold30>0 ~ round(`On.Hand`/QtySold30,1), QtySold90>0 ~ round(`On.Hand`/QtySold90,1), TRUE~NA_real_),
    RecentReceipt= !is.na(Last.Receipt.Date) & Last.Receipt.Date >= Sys.Date()-days(15),
    Reason       = factor(case_when(
      RecentReceipt           ~ 'New Receipt (< 15 days)',
      MonthsOnHand > 6        ~ 'Over 6 months supply',
      QtySold30 == 0          ~ 'No sales in 30 days',
      MonthsOnHand > 3        ~ '> 90 days supply',
      TRUE                    ~ '< 90 days supply'
    ), levels = c('New Receipt (< 15 days)','No sales in 30 days','> 90 days supply','Over 6 months supply','< 90 days supply'))
  )
  })
  
  filteredData <- reactive({
    req(input$reasons)
    df <- inventory()
    if (!('All Manufacturers' %in% input$manufacturer)) df <- filter(df, Manufacturer %in% input$manufacturer)
    if (input$last_purchaser != "All") df <- filter(df, Last.Purchaser == input$last_purchaser)
    df <- filter(df, Reason %in% input$reasons)
    if (input$recent) df <- filter(df, !RecentReceipt)
    if (input$exclude_recent_price_review) {
      df <- filter(df, is.na(Last.Price.Review) | Last.Price.Review < Sys.Date() - days(30))
    }
    df
  })
  
  colour_map <- c(
    '< 90 days supply'        = '#28a745',
    'No sales in 30 days'     = '#FFF3CD',
    '> 90 days supply'        = '#F8D7DA',
    'Over 6 months supply'    = '#FAD7B5',
    'New Receipt (< 15 days)' = '#D4EDDA'
  )
  
  observe({
    inv <- inventory(); req(inv)
    updateSelectInput(session,'manufacturer',choices=c('All Manufacturers',sort(unique(inv$Manufacturer))),selected='All Manufacturers')
    updateSelectInput(session,'last_purchaser',choices=c('All',sort(unique(inv$Last.Purchaser))),selected='All')
    updateCheckboxGroupInput(session,'reasons',choiceNames=make_choice_labels(levels(inv$Reason),colour_map),choiceValues=levels(inv$Reason),selected=levels(inv$Reason))
  })
  
  output$reasonBar <- renderPlot({
    ggplot(filteredData(), aes(Reason, fill=Reason)) +
      geom_bar() +
      scale_fill_manual(values=colour_map) +
      theme_minimal(base_size=14) +
      theme(axis.text.x=element_text(angle=30,hjust=1),legend.position='none') +
      labs(title='Item count by reason',x=NULL,y='Count')
  })
  
  output$monthsHist <- renderPlot({
    ggplot(filteredData(), aes(MonthsOnHand)) +
      geom_histogram(bins=30) +
      theme_minimal(base_size=14) +
      labs(title='Distribution of Months-on-Hand',x='Months',y='Items')
  })
  
  output$manufacturerHeatmap <- renderPlot({
    filteredData() %>%
      count(Manufacturer,Reason) %>%
      pivot_wider(names_from=Reason,values_from=n,values_fill=0) %>%
      pivot_longer(-Manufacturer,names_to='Reason',values_to='Count') %>%
      ggplot(aes(Reason,Manufacturer,fill=Count)) +
      geom_tile() +
      scale_fill_gradient(low='white',high='firebrick') +
      theme_minimal(base_size=14) +
      theme(axis.text.x=element_text(angle=30,hjust=1)) +
      labs(title='Items by Manufacturer & Reason')
  })
  
  output$inventoryTable <- renderDT({
    dat <- filteredData() %>%
      mutate(
        `On.Hand` = replace_na(`On.Hand`, 0),
        `On Order` = replace_na(`On Order`, 0),
        Value = replace_na(Value, 0),
        `Quantity.Sold.in.30.days` = replace_na(`Quantity.Sold.in.30.days`, 0),
        `Quantity.Sold.in.90.days` = replace_na(`Quantity.Sold.in.90.days`, 0),
        MonthsOnHand = replace_na(MonthsOnHand, 0)
      ) %>%
      select(
        `Item Name`           = Item.Name,
        Description,
        Manufacturer,
        `On Hand`             = `On.Hand`,
        `On Order`            = `On Order`,
        Value,
        `Sold (30 days)` = `Quantity.Sold.in.30.days`,
        `Sold (90 days)` = `Quantity.Sold.in.90.days`,
        `Months On Hand`      = MonthsOnHand,
        Reason,
        `Last Price Review`   = Last.Price.Review,
        `Last Receipt Date`   = Last.Receipt.Date
      )
    
    sketch <- htmltools::withTags(table(
      class='display', thead(tr(lapply(names(dat), th))),
      tfoot(tr(lapply(names(dat), function(col) if(col=='Value') th(style='text-align:right;') else th())))
    ))
    
    datatable(
      dat,
      container = sketch,
      rownames  = FALSE,
      class     = 'stripe hover compact',
      options   = list(
        pageLength     = 25,
        lengthMenu     = c(25, 50, 100, 200),
        scrollX        = TRUE,
        autoWidth      = TRUE,
        columnDefs     = list(
          list(className = 'numeric', targets = c(3,4,5,6,7,8))
        ),
        footerCallback = JS(
          "function(row,data,start,end,display){",
          "var api=this.api();",
          "var total=api.column(5,{page:'current'}).data().reduce(function(a,b){",
          "return (Number(a)||0)+(Number(b)||0);},0);",
          "$(api.column(5).footer()).html('$'+total.toFixed(2));}"
        )
      )
    ) %>%
      formatCurrency('Value', '$', digits = 2)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() sprintf('inventory_%s.csv', Sys.Date()),
    content  = function(file) write.csv(filteredData(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
