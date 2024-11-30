# Load required packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(RSQLite)
library(DBI)
library(ggplot2)
library(dplyr)
library(digest)
library(DT)
library(scales)

# Initialize SQLite database
db <- dbConnect(SQLite(), "user_data.sqlite")

# Create users table if it doesn't exist
if (!dbExistsTable(db, "users")) {
  dbExecute(db, "CREATE TABLE users (id INTEGER PRIMARY KEY, email TEXT UNIQUE, password TEXT)")
}

# Create financial data tables if they don't exist
if (!dbExistsTable(db, "assets")) {
  dbExecute(db, "CREATE TABLE assets (id INTEGER PRIMARY KEY, user_id INTEGER, label TEXT, amount REAL, type TEXT)")
}

if (!dbExistsTable(db, "liabilities")) {
  dbExecute(db, "CREATE TABLE liabilities (id INTEGER PRIMARY KEY, user_id INTEGER, label TEXT, amount REAL)")
}

if (!dbExistsTable(db, "expenses")) {
  dbExecute(db, "CREATE TABLE expenses (id INTEGER PRIMARY KEY, user_id INTEGER, label TEXT, amount REAL)")
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        font-size: 16px;
      }
      h1, h2, h3 {
        font-weight: 600;
      }
      .navbar-brand {
        font-size: 24px !important;
      }
      .form-control {
        font-size: 16px;
      }
      .btn {
        font-size: 16px;
        padding: 10px 20px;
      }
      .tab-content {
        padding-top: 20px;
      }
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .dataTable {
        font-size: 16px;
      }
      table.dataTable tbody td {
        padding: 8px;
      }
    "))
  ),
  navbarPage(
    title = "Personal Finance Tracker",
    id = "main_navbar",
    tabPanel("Login/Register",
             uiOutput("auth_ui")
    ),
    tabPanel("Dashboard",
             conditionalPanel(
               condition = "output.userLoggedIn == true",
               tabsetPanel(
                 tabPanel(icon = icon("university"), "Assets",
                          fluidRow(
                            column(4,
                                   textInput("asset_label", "Asset Label"),
                                   numericInput("asset_amount", "Amount", value = NULL),
                                   pickerInput("asset_type", "Asset Type", choices = c("Real Estate", "Cash", "Stocks + Bonds"))
                            ),
                            column(2,
                                   br(),
                                   actionButton("add_asset", "Add Asset", icon = icon("plus-circle"), class = "btn-success")
                            )
                          ),
                          br(),
                          DTOutput("asset_table")
                 ),
                 tabPanel(icon = icon("credit-card"), "Liabilities",
                          fluidRow(
                            column(4,
                                   textInput("liability_label", "Liability Label"),
                                   numericInput("liability_amount", "Amount", value = NULL)
                            ),
                            column(2,
                                   br(),
                                   actionButton("add_liability", "Add Liability", icon = icon("plus-circle"), class = "btn-danger")
                            )
                          ),
                          br(),
                          DTOutput("liability_table")
                 ),
                 tabPanel(icon = icon("shopping-cart"), "Expenses",
                          fluidRow(
                            column(4,
                                   textInput("expense_label", "Expense Label"),
                                   numericInput("expense_amount", "Amount", value = NULL)
                            ),
                            column(2,
                                   br(),
                                   actionButton("add_expense", "Add Expense", icon = icon("plus-circle"), class = "btn-warning")
                            )
                          ),
                          br(),
                          DTOutput("expense_table")
                 ),
                 tabPanel(icon = icon("chart-pie"), "Summary",
                          br(),
                          fluidRow(
                            column(12,
                                   h3("Assets"),
                                   DTOutput("assets_summary_table"),
                                   br(),
                                   h3("Liabilities"),
                                   DTOutput("liabilities_summary_table"),
                                   br(),
                                   h3("Expenses"),
                                   DTOutput("expenses_summary_table"),
                                   br(),
                                   h3("Total Summary"),
                                   tableOutput("summary_table"),
                                   br(),
                                   h3("Asset Allocation Percentage Breakdown"),
                                   tableOutput("asset_percentage_table"),
                                   br(),
                                   h3("Visualizations"),
                                   fluidRow(
                                     column(6, plotOutput("asset_pie_chart")),
                                     column(6, plotOutput("liability_pie_chart"))
                                   )
                            )
                          )
                 ),
                 tabPanel(icon = icon("sign-out-alt"), "Logout",
                          br(),
                          actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), class = "btn-primary btn-lg")
                 )
               )
             )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive value to store user login status
  user_logged_in <- reactiveVal(FALSE)
  
  # Reactive value to store user email
  user_email <- reactiveVal(NULL)
  
  # Export user login status to UI
  output$userLoggedIn <- reactive({ user_logged_in() })
  outputOptions(output, "userLoggedIn", suspendWhenHidden = FALSE)
  
  # Authentication UI
  output$auth_ui <- renderUI({
    if (!user_logged_in()) {
      fluidRow(
        column(4, offset = 4,
               tabsetPanel(
                 tabPanel("Login",
                          br(),
                          textInput("login_email", "Email"),
                          passwordInput("login_password", "Password"),
                          actionButton("login_btn", "Login", class = "btn-primary btn-lg", icon = icon("sign-in-alt"))
                 ),
                 tabPanel("Register",
                          br(),
                          textInput("register_email", "Email"),
                          passwordInput("register_password", "Password"),
                          actionButton("register_btn", "Register", class = "btn-success btn-lg", icon = icon("user-plus"))
                 ),
                 tabPanel("Reset Password",
                          br(),
                          textInput("reset_email", "Email"),
                          passwordInput("reset_password", "New Password"),
                          actionButton("reset_btn", "Reset Password", class = "btn-warning btn-lg", icon = icon("key"))
                 )
               )
        )
      )
    }
  })
  
  # Login action
  observeEvent(input$login_btn, {
    req(input$login_email, input$login_password)
    
    # Hash the password
    hashed_password <- digest(input$login_password, algo = "sha256")
    
    user <- dbGetQuery(db, "SELECT * FROM users WHERE email = ? AND password = ?", params = list(input$login_email, hashed_password))
    if (nrow(user) == 1) {
      user_logged_in(TRUE)
      user_email(input$login_email)
      updateNavbarPage(session, "main_navbar", selected = "Dashboard")
    } else {
      showModal(modalDialog("Invalid email or password", easyClose = TRUE))
    }
  })
  
  # Register action
  observeEvent(input$register_btn, {
    req(input$register_email, input$register_password)
    
    # Check if email is already registered
    existing_user <- dbGetQuery(db, "SELECT * FROM users WHERE email = ?", params = list(input$register_email))
    if (nrow(existing_user) == 0) {
      # Hash the password
      hashed_password <- digest(input$register_password, algo = "sha256")
      
      dbExecute(db, "INSERT INTO users (email, password) VALUES (?, ?)", params = list(input$register_email, hashed_password))
      showModal(modalDialog("Registration successful! You can now log in.", easyClose = TRUE))
    } else {
      showModal(modalDialog("Email already registered", easyClose = TRUE))
    }
  })
  
  # Reset Password action
  observeEvent(input$reset_btn, {
    req(input$reset_email, input$reset_password)
    
    # Check if email exists
    existing_user <- dbGetQuery(db, "SELECT * FROM users WHERE email = ?", params = list(input$reset_email))
    if (nrow(existing_user) == 1) {
      # Hash the new password
      hashed_password <- digest(input$reset_password, algo = "sha256")
      
      dbExecute(db, "UPDATE users SET password = ? WHERE email = ?", params = list(hashed_password, input$reset_email))
      showModal(modalDialog("Password reset successful! You can now log in with your new password.", easyClose = TRUE))
    } else {
      showModal(modalDialog("Email not found", easyClose = TRUE))
    }
  })
  
  # Logout action
  observeEvent(input$logout_btn, {
    user_logged_in(FALSE)
    user_email(NULL)
    updateNavbarPage(session, "main_navbar", selected = "Login/Register")
  })
  
  # Reactive values to trigger table updates
  asset_trigger <- reactiveVal(0)
  liability_trigger <- reactiveVal(0)
  expense_trigger <- reactiveVal(0)
  
  # Add Asset
  observeEvent(input$add_asset, {
    req(user_logged_in())
    req(!is.null(input$asset_amount) && input$asset_amount != 0)
    req(input$asset_type)
    
    # Provide default label if empty
    asset_label <- ifelse(nchar(input$asset_label) > 0, input$asset_label, "Unnamed Asset")
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    dbExecute(db, "INSERT INTO assets (user_id, label, amount, type) VALUES (?, ?, ?, ?)",
              params = list(user_id, asset_label, input$asset_amount, input$asset_type))
    
    # Increment the asset trigger to update the table
    asset_trigger(asset_trigger() + 1)
    
    # Clear input fields
    updateTextInput(session, "asset_label", value = "")
    updateNumericInput(session, "asset_amount", value = NULL)
    updatePickerInput(session, "asset_type", selected = NULL)
  })
  
  # Add Liability
  observeEvent(input$add_liability, {
    req(user_logged_in())
    req(!is.null(input$liability_amount) && input$liability_amount != 0)
    
    # Provide default label if empty
    liability_label <- ifelse(nchar(input$liability_label) > 0, input$liability_label, "Unnamed Liability")
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    dbExecute(db, "INSERT INTO liabilities (user_id, label, amount) VALUES (?, ?, ?)",
              params = list(user_id, liability_label, input$liability_amount))
    
    # Increment the liability trigger to update the table
    liability_trigger(liability_trigger() + 1)
    
    # Clear input fields
    updateTextInput(session, "liability_label", value = "")
    updateNumericInput(session, "liability_amount", value = NULL)
  })
  
  # Add Expense
  observeEvent(input$add_expense, {
    req(user_logged_in())
    req(!is.null(input$expense_amount) && input$expense_amount != 0)
    
    # Provide default label if empty
    expense_label <- ifelse(nchar(input$expense_label) > 0, input$expense_label, "Unnamed Expense")
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    dbExecute(db, "INSERT INTO expenses (user_id, label, amount) VALUES (?, ?, ?)",
              params = list(user_id, expense_label, input$expense_amount))
    
    # Increment the expense trigger to update the table
    expense_trigger(expense_trigger() + 1)
    
    # Clear input fields
    updateTextInput(session, "expense_label", value = "")
    updateNumericInput(session, "expense_amount", value = NULL)
  })
  
  # Render Asset Table with Delete Button
  output$asset_table <- renderDT({
    req(user_logged_in())
    asset_trigger()  # Depend on asset_trigger to refresh table
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    assets <- dbGetQuery(db, "SELECT id, label, amount, type FROM assets WHERE user_id = ?", params = list(user_id))
    
    if (nrow(assets) > 0) {
      # Add delete buttons with data-id attribute
      assets$Delete <- sprintf('<button class="btn btn-danger btn-sm delete_btn" data-id="%s">Delete</button>', assets$id)
    }
    
    assets$amount <- dollar(assets$amount)
    
    datatable(assets, escape = FALSE, selection = 'none', rownames = FALSE, options = list(
      dom = 't',
      paging = FALSE,
      columnDefs = list(list(targets = ncol(assets) - 1, orderable = FALSE))
    ), callback = JS(
      "table.on('click', '.delete_btn', function() {",
      "  var data_id = $(this).data('id');",
      "  Shiny.setInputValue('delete_asset_id', data_id, {priority: 'event'});",
      "});"
    ))
  }, server = FALSE)
  
  # Handle delete actions for assets
  observeEvent(input$delete_asset_id, {
    req(user_logged_in())
    asset_id <- input$delete_asset_id
    dbExecute(db, "DELETE FROM assets WHERE id = ?", params = list(asset_id))
    # Update the asset trigger to refresh the table and observers
    asset_trigger(asset_trigger() + 1)
  })
  
  # Render Liability Table with Delete Button
  output$liability_table <- renderDT({
    req(user_logged_in())
    liability_trigger()  # Depend on liability_trigger to refresh table
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    liabilities <- dbGetQuery(db, "SELECT id, label, amount FROM liabilities WHERE user_id = ?", params = list(user_id))
    
    if (nrow(liabilities) > 0) {
      # Add delete buttons with data-id attribute
      liabilities$Delete <- sprintf('<button class="btn btn-danger btn-sm delete_liability_btn" data-id="%s">Delete</button>', liabilities$id)
    }
    
    liabilities$amount <- dollar(liabilities$amount)
    
    datatable(liabilities, escape = FALSE, selection = 'none', rownames = FALSE, options = list(
      dom = 't',
      paging = FALSE,
      columnDefs = list(list(targets = ncol(liabilities) - 1, orderable = FALSE))
    ), callback = JS(
      "table.on('click', '.delete_liability_btn', function() {",
      "  var data_id = $(this).data('id');",
      "  Shiny.setInputValue('delete_liability_id', data_id, {priority: 'event'});",
      "});"
    ))
  }, server = FALSE)
  
  # Handle delete actions for liabilities
  observeEvent(input$delete_liability_id, {
    req(user_logged_in())
    liability_id <- input$delete_liability_id
    dbExecute(db, "DELETE FROM liabilities WHERE id = ?", params = list(liability_id))
    # Update the liability trigger to refresh the table and observers
    liability_trigger(liability_trigger() + 1)
  })
  
  # Render Expense Table with Delete Button
  output$expense_table <- renderDT({
    req(user_logged_in())
    expense_trigger()  # Depend on expense_trigger to refresh table
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    expenses <- dbGetQuery(db, "SELECT id, label, amount FROM expenses WHERE user_id = ?", params = list(user_id))
    
    if (nrow(expenses) > 0) {
      # Add delete buttons with data-id attribute
      expenses$Delete <- sprintf('<button class="btn btn-danger btn-sm delete_expense_btn" data-id="%s">Delete</button>', expenses$id)
    }
    
    expenses$amount <- dollar(expenses$amount)
    
    datatable(expenses, escape = FALSE, selection = 'none', rownames = FALSE, options = list(
      dom = 't',
      paging = FALSE,
      columnDefs = list(list(targets = ncol(expenses) - 1, orderable = FALSE))
    ), callback = JS(
      "table.on('click', '.delete_expense_btn', function() {",
      "  var data_id = $(this).data('id');",
      "  Shiny.setInputValue('delete_expense_id', data_id, {priority: 'event'});",
      "});"
    ))
  }, server = FALSE)
  
  # Handle delete actions for expenses
  observeEvent(input$delete_expense_id, {
    req(user_logged_in())
    expense_id <- input$delete_expense_id
    dbExecute(db, "DELETE FROM expenses WHERE id = ?", params = list(expense_id))
    # Update the expense trigger to refresh the table and observers
    expense_trigger(expense_trigger() + 1)
  })
  
  # Assets Summary Table
  output$assets_summary_table <- renderDT({
    req(user_logged_in())
    asset_trigger()
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    assets <- dbGetQuery(db, "SELECT label AS Label, type AS Type, amount AS Amount FROM assets WHERE user_id = ?", params = list(user_id))
    
    if (nrow(assets) > 0) {
      # Calculate total assets before formatting
      total_assets <- sum(assets$Amount)
      # Format the Amount column as US currency
      assets$Amount <- dollar(assets$Amount)
      total_row <- data.frame(Label = "Total Assets", Type = "", Amount = dollar(total_assets))
      assets <- rbind(assets, total_row)
    }
    
    datatable(assets, options = list(dom = 't', paging = FALSE), rownames = FALSE) %>%
      formatStyle('Label', fontWeight = styleEqual("Total Assets", "bold")) %>%
      formatStyle(columns = names(assets), backgroundColor = 'lightblue', target = 'row', rows = nrow(assets))
  })
  
  # Liabilities Summary Table
  output$liabilities_summary_table <- renderDT({
    req(user_logged_in())
    liability_trigger()
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    liabilities <- dbGetQuery(db, "SELECT label AS Label, amount AS Amount FROM liabilities WHERE user_id = ?", params = list(user_id))
    
    if (nrow(liabilities) > 0) {
      total_liabilities <- sum(liabilities$Amount)
      liabilities$Amount <- dollar(liabilities$Amount)
      total_row <- data.frame(Label = "Total Liabilities", Amount = dollar(total_liabilities))
      liabilities <- rbind(liabilities, total_row)
    }
    
    datatable(liabilities, options = list(dom = 't', paging = FALSE), rownames = FALSE) %>%
      formatStyle('Label', fontWeight = styleEqual("Total Liabilities", "bold")) %>%
      formatStyle(columns = names(liabilities), backgroundColor = 'lightpink', target = 'row', rows = nrow(liabilities))
  })
  
  # Expenses Summary Table
  output$expenses_summary_table <- renderDT({
    req(user_logged_in())
    expense_trigger()
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    expenses <- dbGetQuery(db, "SELECT label AS Label, amount AS Amount FROM expenses WHERE user_id = ?", params = list(user_id))
    
    if (nrow(expenses) > 0) {
      total_expenses <- sum(expenses$Amount)
      expenses$Amount <- dollar(expenses$Amount)
      total_row <- data.frame(Label = "Total Expenses", Amount = dollar(total_expenses))
      expenses <- rbind(expenses, total_row)
    }
    
    datatable(expenses, options = list(dom = 't', paging = FALSE), rownames = FALSE) %>%
      formatStyle('Label', fontWeight = styleEqual("Total Expenses", "bold")) %>%
      formatStyle(columns = names(expenses), backgroundColor = 'lightgreen', target = 'row', rows = nrow(expenses))
  })
  
  # Total Summary Table
  output$summary_table <- renderTable({
    req(user_logged_in())
    asset_trigger()
    liability_trigger()
    expense_trigger()
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    
    # Total assets
    assets_total <- dbGetQuery(db, "SELECT SUM(amount) as total_assets FROM assets WHERE user_id = ?", params = list(user_id))$total_assets
    assets_total <- ifelse(is.na(assets_total), 0, assets_total)
    
    # Total liabilities
    liabilities_total <- dbGetQuery(db, "SELECT SUM(amount) as total_liabilities FROM liabilities WHERE user_id = ?", params = list(user_id))$total_liabilities
    liabilities_total <- ifelse(is.na(liabilities_total), 0, liabilities_total)
    
    # Net worth
    net_worth <- assets_total - liabilities_total
    
    # Total cash amount
    cash_total <- dbGetQuery(db, "SELECT SUM(amount) as cash_total FROM assets WHERE user_id = ? AND type = 'Cash'", params = list(user_id))$cash_total
    cash_total <- ifelse(is.na(cash_total), 0, cash_total)
    
    # Total monthly expenses
    expenses_total <- dbGetQuery(db, "SELECT SUM(amount) as total_expenses FROM expenses WHERE user_id = ?", params = list(user_id))$total_expenses
    expenses_total <- ifelse(is.na(expenses_total), 0, expenses_total)
    
    # Total monthly expenses plus $1,500
    total_monthly_expenses <- expenses_total + 1500
    
    # Number of months in savings
    if (total_monthly_expenses > 0) {
      months_in_savings <- cash_total / total_monthly_expenses
      months_in_savings <- round(months_in_savings, 2)
    } else {
      months_in_savings <- NA
    }
    
    # Create data frame
    summary_df <- data.frame(
      Metric = c("Total Assets", "Total Liabilities", "Net Worth", "Number of Months in Savings"),
      Amount = c(dollar(assets_total), dollar(liabilities_total), dollar(net_worth), months_in_savings)
    )
    
    summary_df
  })
  
  # Asset Percentage Breakdown Table
  output$asset_percentage_table <- renderTable({
    req(user_logged_in())
    asset_trigger()
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    
    asset_data <- dbGetQuery(db, "SELECT type, SUM(amount) as total_amount FROM assets WHERE user_id = ? GROUP BY type", params = list(user_id))
    
    total_assets <- sum(asset_data$total_amount)
    
    if (total_assets > 0) {
      asset_data$Percentage <- round((asset_data$total_amount / total_assets) * 100, 2)
      asset_data$Total_Amount <- dollar(asset_data$total_amount)
      asset_data <- asset_data %>% select(Type = type, Amount = Total_Amount, Percentage)
    } else {
      asset_data <- data.frame(Type = character(), Amount = character(), Percentage = numeric())
    }
    
    asset_data
  })
  
  # Asset Pie Chart
  output$asset_pie_chart <- renderPlot({
    req(user_logged_in())
    asset_trigger()  # Refresh when assets change
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    asset_data <- dbGetQuery(db, "SELECT type, SUM(amount) as total_amount FROM assets WHERE user_id = ? GROUP BY type", params = list(user_id))
    
    if (nrow(asset_data) > 0) {
      asset_data$Percentage <- asset_data$total_amount / sum(asset_data$total_amount) * 100
      asset_data$Label <- paste0(asset_data$type, "\n", dollar(asset_data$total_amount), "\n", round(asset_data$Percentage, 2), "%")
      
      ggplot(asset_data, aes(x = "", y = total_amount, fill = type)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        labs(title = "Asset Allocation", x = NULL, y = NULL, fill = "Asset Type") +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 18, face = "bold")
        )
    }
  })
  
  # Liability Pie Chart
  output$liability_pie_chart <- renderPlot({
    req(user_logged_in())
    liability_trigger()  # Refresh when liabilities change
    
    user_id <- dbGetQuery(db, "SELECT id FROM users WHERE email = ?", params = list(user_email()))$id
    liability_data <- dbGetQuery(db, "SELECT label, amount FROM liabilities WHERE user_id = ?", params = list(user_id))
    
    if (nrow(liability_data) > 0) {
      liability_data$Percentage <- liability_data$amount / sum(liability_data$amount) * 100
      liability_data$Label <- paste0(liability_data$label, "\n", dollar(liability_data$amount), "\n", round(liability_data$Percentage, 2), "%")
      
      ggplot(liability_data, aes(x = "", y = amount, fill = label)) +
        geom_col(width = 1, color = "white") +
        coord_polar(theta = "y") +
        labs(title = "Liability Breakdown", x = NULL, y = NULL, fill = "Liability") +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 18, face = "bold")
        )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
