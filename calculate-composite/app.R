library(shiny)
library(dplyr)
library(readr)

# Define UI
ui <- fluidPage(
  
  title = "Composite Score PCA",
  div(style = "text-align: center;", 
      h1("Cognitive Composite Score Calculator using PCA")),
  h5("Conceptualized by:", 
     tags$a(href = "https://www.linkedin.com/in/raghav-prasad-neurology/", "Raghav Prasad"),
     "Designed by:",
     tags$a(href = "https://www.linkedin.com/in/pradhanhitesh/", "Hitesh Pradhan"),
     id = "bottom_text",
     style = "color: red; text-align: center;"),
  #h5("Motivation papers: ",
     #tags$a(href = "https://github.com/pradhanhitesh/Characterisitcs-Table/issues", "[1]"),
     #tags$a(href = "https://github.com/pradhanhitesh/Characterisitcs-Table", "[2]"),
     #align = 'center'),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      uiOutput("checkboxes"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      verbatimTextOutput("loadings")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read data from uploaded file
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    return(df)
  })
  
  # Dynamically create checkboxes for column selection
  output$checkboxes <- renderUI({
    colnames <- names(data())
    checkboxGroupInput("columns", "Select Columns", choices = colnames)
  })
  
  # Perform PCA and display loadings
  output$loadings <- renderPrint({
    req(input$columns)
    sub_data <- data() %>%
      select(input$columns)
    scaled_data <- scale(sub_data)
    pca <- prcomp(scaled_data, center = FALSE, scale. = FALSE)
    loadings <- pca$rotation[, 1]
    loadings
  })
  
  # Modify data and allow download
  output$downloadData <- downloadHandler(
    filename = function() {
      "composite_score_added_data.csv"
    },
    content = function(file) {
      req(input$columns)
      sub_data <- data() %>%
        select(input$columns)
      scaled_data <- scale(sub_data)
      pca <- prcomp(scaled_data, center = FALSE, scale. = FALSE)
      loadings <- pca$rotation[, 1]
      data_with_global <- mutate(data(), Composite_Score = rowSums(sweep(scaled_data, 2, loadings, "*")))
      write.csv(data_with_global, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
