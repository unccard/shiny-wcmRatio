library(shiny)
library(tidyr)
source("functions.R")

ui <- fluidPage(
  # App title 
  headerPanel("Word Complexity Measure"),
  
  # Sidebar panel with inputs 
  sidebarPanel(
    textAreaInput("target", "Target Transcript:", placeholder="Paste Klattese targets here...", height = '250px', width = "100%"), 
    textAreaInput("production", "Production Transcript", placeholder="Paste Klattese productions here...", height = '250px', width = "100%"),
    checkboxInput("isMarked", "Select this box if transcripts are marked for stress and syllables", value=FALSE),
    actionButton("submit", "Calculate WCM Ratio")
  ),
  
  # Main panel with outputs
  mainPanel(
    DT::dataTableOutput("word_by_word", "auto", "auto"), 
    DT::dataTableOutput("average", "auto", "auto")
  )
)

server <- function(input, output) {
  vals <- reactiveValues()  # stores values that will be updated and accessed throughout app
  word_db <- read.csv('UNCWordDB-2021-10-08.csv', na.strings=c("", "NA"))
  vals$tibbletest <- tibble(word_db$KlatteseSyll, word_db$KlatteseBare, word_db$Zipf.value) # isolate the categories we need from word_db
  
  observeEvent(input$submit, {
    # must have values for both target and production
    req(input$target)  
    req(input$production)
    
    # initialize total vars
    vals$target_total <- vals$prod_total <- vals$ratio_total <- vals$error_total <- 
      vals$accuracy_total <- vals$wf_total <- 0 
    vals$wbw_row <- 1
    
    # store inputs in reactive 
    vals$target <- cleanSample(input$target)
    vals$prod <- cleanSample(input$production)
    vals$isMarked <- input$isMarked
    
    # perform calculations and store in outputs
    vals$word_by_word <- updateWordByWord(vals)
    vals$avg_data <- updateAverage(vals)
    
    # display the word by word output
    output$word_by_word <- DT::renderDataTable(
      vals$word_by_word, caption = "Word by Word",
      server = TRUE
    )
    
    # display the average output
    output$average <- DT::renderDataTable (
      vals$avg_data, caption = "Average",
      server = TRUE
    )
  })
}

shinyApp(ui, server)
