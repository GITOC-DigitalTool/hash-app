
# Check and install required packages if not already installed
required_packages <- c("shiny", "bslib", "digest", "readxl")
installed <- installed.packages()[, "Package"]
for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

library(shiny)
library(bslib)
library(digest)
library(readxl)

shared_key <- "HC_RandomGen_Seed" # do not change this, it is the random seed code that enables everyone to produce that same randomly generated hash codes for their URLS


ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"),
  titlePanel("URL Hasher"),
  
  # Custom CSS to style the text input boxes
  tags$style(HTML("
    .form-control {
      background-color: white !important;
      color: black;
    }
  ")),
  
  tabsetPanel(
    tabPanel("Single URL",
             br(),
             fluidRow(
               column(6, offset = 3,
                      wellPanel(
                        textInput("url_input", "Enter URL", placeholder = "https://example.com"),
                        verbatimTextOutput("hashed_url"),
                        actionButton("copy_btn", "Copy to Clipboard", icon = icon("copy"), class = "btn-primary")
                      )
               )
             )
    ),
    tabPanel("Batch Upload",
             br(),
             fluidRow(
               column(6, offset = 3,
                      wellPanel(
                        tags$p("Upload a .csv file with a single column of your URLs here"),
                        textInput("org_name", "Organization Name", placeholder = "Enter Organization Name"),
                        fileInput("file_upload", "Upload CSV or XLSX file", accept = c(".csv", ".xlsx")),
                        downloadButton("download_file", "Download Processed File", class = "btn-success")
                      )
               )
             ),
             fluidRow(
               column(6, offset = 3,
                      wellPanel(
                        h4("Summary"),
                        textOutput("num_records")
                      )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  hash_result <- reactive({
    if (nzchar(input$url_input)) {
      hmac(key = shared_key, object = trimws(tolower(input$url_input)), algo = "sha256")
    } else {
      ""
    }
  })
  
  output$hashed_url <- renderText({
    hash_result()
  })
  
  file_data <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext == "csv") {
      read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      read_excel(input$file_upload$datapath)
    } else {
      showNotification("Invalid file type. Please upload a CSV or XLSX file.", type = "error")
      return(NULL)
    }
  })
  
  processed_file <- reactive({
    df <- file_data()
    req(df)
    if (ncol(df) < 1) return(NULL)
    
    col_name <- names(df)[1]
    df$hashed_URL <- sapply(df[[col_name]], function(x) {
      x <- as.character(x)
      if (!is.na(x) && nzchar(trimws(x))) {
        hmac(key = shared_key, object = trimws(x), algo = "sha256")
      } else {
        NA
      }
    })
    
    df$Org_Name <- ifelse(nzchar(input$org_name), input$org_name, "Unknown")
    names(df)[1] <- "URL"
    
    # Show notification after processing
    showNotification("Data Encrypted!", type = "message", duration = 3)
    
    df
  })
  
  output$num_records <- renderText({
    df <- file_data()
    if (is.null(df)) return("Number of records: 0")
    paste("Number of records encrypted:", nrow(df))
  })
  
  output$download_file <- downloadHandler(
    filename = function() {
      paste0("hashed_urls_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(processed_file(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)