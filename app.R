library(shiny)
library(tidyr)
library(stringr)
source("functions.R")

ui <- fluidPage(
  # App title 
  headerPanel("Word Complexity Measure Ratio"),
  
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
    downloadButton("downloadWBW", "Download"), 
    HTML("<hr>"),
    DT::dataTableOutput("average", "auto", "auto"), 
    downloadButton("downloadAVG", "Download"),
  ), 
  
  HTML("<hr>"), 
  
  div(
    class = "footer", 
    titlePanel("Notes:"),
    tags$ul(
      tags$li("Be sure to input both target and production before running calculations."),
      tags$li("Your input may be separated by space or newline characters."), 
      tags$li("To mark syllables and stress, use - as the syllable boundary and ˈ or ' to mark primary stress."), 
      tags$li("This app does not save data between calculations. Be sure to use the download buttons if you need to save your data."), 
      tags$li("For more information on WCM, Zipf Frequency, and our database, refer to our GitHub."),
      tags$a(href="https://github.com/unccard/shiny-wcmRatio", "https://github.com/unccard/shiny-wcmRatio")
      
    )
  ),

)

server <- function(input, output) {
  vals <- reactiveValues()  # stores values that will be updated and accessed throughout app
  word_db <- read.csv('UNCWordDB-2022-02-07.csv', na.strings=c("", "NA"))
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
    print(vals$isMarked)
    
    # perform calculations and store in outputs
    vals$word_by_word <- updateWordByWord(vals)
    vals$avg_data <- updateAverage(vals)
  
    if(nrow(vals$word_by_word) > 0) {  
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
    
    output$downloadWBW <- downloadHandler(
      filename = function(){
        "word_by_word.csv"
      },
      content = function(file) {
        write.csv(vals$word_by_word,file)
      }
    )
    
    output$downloadAVG <- downloadHandler(
      filename = function() {
        "avg_data.csv"
      },
      content = function(file) {
        write.csv(vals$avg_data, file)
      }
    )
    } else {
      output$word_by_word <- renderText("No table to display")
    }
    
  })
}

shinyApp(ui, server)
