library(shiny)
source("functions.R")

ui <- fluidPage(
  # App title 
  headerPanel("Word Complexity Measure"),
  
  # Sidebar panel with inputs 
  sidebarPanel(
    textAreaInput("target", "Target Transcript:", placeholder="Paste Klattese targets here...", height = '250px', width = "100%"), 
    textAreaInput("production", "Production Transcript", placeholder="Paste Klattese productions here...", height = '250px', width = "100%"),
    checkboxInput("isMarked", "Check if transcripts are marked for stress and syllables", value=FALSE),
    actionButton("submit", "Calculate WCM Ratio")
  ),
  
  # Main panel with outputs
  mainPanel(
    DT::dataTableOutput("word_by_word", "auto", "auto"), 
    DT::dataTableOutput("average", "auto", "auto")
  )
)

server <- function(input, output) {
  word_db <- read.csv('UNCWordDB-2021-10-08.csv', na.strings=c("", "NA"))
  tibbletest <-tibble(word_db$KlatteseSyll, word_db$KlatteseBare, word_db$Zipf.value) # isolate the categories we need from word_db

  vals <- reactiveValues()  # stores values that will be updated and accessed throughout app 
  
}

shinyApp(ui, server)
