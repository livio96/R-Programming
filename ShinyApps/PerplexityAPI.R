library(shiny)
library(httr)
library(jsonlite)

ui <- fluidPage(
  titlePanel("Perplexity AI API Demo"),
  sidebarLayout(
    sidebarPanel(
      textInput("question", "Enter your question:", ""),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("response")
    )
  )
)

server <- function(input, output) {
  
  answer_text <- reactiveVal("")
  
  observeEvent(input$submit, {
    req(input$question)
    
    # Replace <token> with your actual API token
    api_token <- ""
    
    url <- "https://api.perplexity.ai/chat/completions"
    
    headers <- add_headers(
      Authorization = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    )
    
    body <- list(
      model = "llama-3.1-sonar-small-128k-online",
      messages = list(
        list(
          role = "system",
          content = "Be very technical"
        ),
        list(
          role = "user",
          content = input$question
        )
      ),
      temperature = 0.2,
      top_p = 0.9,
      return_citations = TRUE,
      search_domain_filter = list("perplexity.ai"),
      return_images = FALSE,
      return_related_questions = FALSE,
      search_recency_filter = "month",
      top_k = 0,
      stream = FALSE,
      presence_penalty = 0,
      frequency_penalty = 1
    )
    
    # Convert body to JSON
    body_json <- toJSON(body, auto_unbox = TRUE)
    
    # Make the POST request
    res <- POST(url, headers, body = body_json)
    
    # Check the status
    if (status_code(res) == 200) {
      res_content <- content(res, as = "parsed", type = "application/json")
      # Extract the answer
      answer <- res_content$choices[[1]]$message$content
      answer_text(answer)
    } else {
      error_message <- paste("Error:", status_code(res), content(res, "text", encoding = "UTF-8"))
      answer_text(error_message)
    }
    
  })
  
  output$response <- renderText({
    answer_text()
  })
  
}

shinyApp(ui = ui, server = server)
