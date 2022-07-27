#' wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import wordcloud RColorBrewer
#' 
mod_wordcloud_ui <- function(id, df){
  ns <- NS(id)
  var_options1 <- c("Financial difficulties" = "fin",
                   "Conflicts with clients, stakeholders or colleagues" = "wit",
                   "Conflicts between clients, stakeholders, or colleagues" = "bet",
                   "Legal issues" = "leg",
                   "Absence or a lack of personnel or support" = "abs",
                   "Problems related to material/ service supply or quality" = "mat",
                   "Mistakes or mishaps" = "mis",
                   "Other" = "oth")
  shiny::tagList(
    selectInput(ns("selection"),
                label = "Choose an event category:",
                choices = var_options1,
                selected = var_options1[1]),
    actionButton(ns("update"), 
                 "Change"),
    hr(),
    sliderInput(ns("freq"),
                "Minimum Frequency:",
                min = 1,  max = 5, value = 1),
    sliderInput(ns("max"),
                "Maximum Number of Words:",
                min = 1,  max = 20,  value = 13),
    plotOutput(ns("plot"))
    )
  }
    
#' wordcloud Server Functions
#'
#' @noRd 
mod_wordcloud_server <- function(id, df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Filter data based on selections
    filtered_rows <- reactive({
      data <- df
      if (input$selection == "fin") {
        data <- data[data$`Severest event` == 1,]
      }
      if (input$selection == "wit") {
        data <- data[data$`Severest event` == 2,]
      }
      if (input$selection == "bet") {
        data <- data[data$`Severest event` == 3,]
      }
      if (input$selection == "leg") {
        data <- data[data$`Severest event` == 4,]
      }
      if (input$selection == "abs") {
        data <- data[data$`Severest event` == 5,]
      }
      if (input$selection == "mat") {
        data <- data[data$`Severest event` == 6,]
      }
      if (input$selection == "mis") {
        data <- data[data$`Severest event` == 7,]
      }
      if (input$selection == "oth") {
        data <- data[data$`Severest event` == 8,]
      }
      data %>% select(`Event description`)
    })
    # Define a reactive expression for the document term matrix
    terms <- reactive({
      # Change when the "update" button is pressed...
      input$update
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(filtered_rows())
        })
      })
    })
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud::wordcloud)
    
    output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale = c(4,0.5),
                    min.freq = input$freq, max.words = input$max,
                    colors = RColorBrewer::brewer.pal(8, "Dark2"))
    })
  })
}
    
## To be copied in the UI
# mod_wordcloud_ui("wordcloud_1")
    
## To be copied in the server
# mod_wordcloud_server("wordcloud_1")
