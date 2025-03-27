
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

shared_key <- "PlaceholderSecretKey"

ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"),
  titlePanel("URL Hasher"),
  
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
                        fileInput("file_upload", "Upload CSV or XLSX file", accept = c(".csv", ".xlsx")),
                        downloadButton("download_file", "Download Processed File", class = "btn-success")
                      )
               )
             )
    )
  ),
  
  tags$script(HTML(
    "$(document).ready(function(){
      $('#copy_btn').click(function(){
        var text = $('#hashed_url').text();
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text)
            .then(function() {
              console.log('Text successfully copied using Clipboard API!');
            })
            .catch(function(err) {
              console.error('Clipboard API failed, trying fallback method:', err);
              fallbackCopyText(text);
            });
        } else {
          fallbackCopyText(text);
        }
      });
      
      function fallbackCopyText(text) {
        var textArea = document.createElement('textarea');
        textArea.value = text;
        // Avoid scrolling to bottom
        textArea.style.top = '0';
        textArea.style.left = '0';
        textArea.style.position = 'fixed';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        try {
          var successful = document.execCommand('copy');
          var msg = successful ? 'successful' : 'unsuccessful';
          console.log('Fallback: Copying text command was ' + msg);
        } catch (err) {
          console.error('Fallback: Unable to copy', err);
        }
        document.body.removeChild(textArea);
      }
    });"
  ))
)

server <- function(input, output, session) {
  
  hash_result <- reactive({
    if (nzchar(input$url_input)) {
      hmac(key = shared_key, object = trimws(input$url_input), algo = "sha256")
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
    if ("url" %in% names(df)) {
      col_name <- "url"
    } else {
      col_name <- names(df)[1]
    }
    df[[paste0(col_name, "_hashed")]] <- sapply(df[[col_name]], function(x) {
      x <- as.character(x)
      if (!is.na(x) && nzchar(trimws(x))) {
        hmac(key = shared_key, object = trimws(x), algo = "sha256")
      } else {
        NA
      }
    })
    df
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
