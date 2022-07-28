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
    div(style = "display: inline-block; width: 300px;",
    selectInput(ns("selection"),
                label = "Choose an event category:",
                choices = var_options1,
                selected = var_options1[1])),
    br(),
    actionButton(ns("update"), 
                 "Update",
                 icon("refresh"), 
                 style = "color: #ffffff; background-color: #337ab7; border-color: #2e6da4"),
    fluidRow(
      column(
        6,
        plotOutput(ns("plot"))
      ),
      column(
        3,
        offset = 0.5,
        br(),
        br(),
        br(),
        br(),
        br(),
        sliderInput(ns("freq"),
                    "Minimum Frequency:",
                    min = 1,  max = 4, value = 1),
        sliderInput(ns("max"),
                    "Maximum Number of Words:",
                    min = 1,  max = 15,  value = 15)
        )
      ),
    fluidRow(
      column(
        6,
        tagList(
          uiOutput(ns("help_text"))
        )
      )
    )
  )
  }
    
#' wordcloud Server Functions
#'
#' @noRd 
mod_wordcloud_server <- function(id, df, partID){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Filter data based on selections
    filtered_rows <- reactive({
      data <- df %>% sjlabelled::remove_all_labels(.)
      if (input$selection == "fin") {
        data <- data[data$Severest.event == 1,]
      }
      if (input$selection == "wit") {
        data <- data[data$Severest.event == 2,]
      }
      if (input$selection == "bet") {
        data <- data[data$Severest.event == 3,]
      }
      if (input$selection == "leg") {
        data <- data[data$Severest.event == 4,]
      }
      if (input$selection == "abs") {
        data <- data[data$Severest.event == 5,]
      }
      if (input$selection == "mat") {
        data <- data[data$Severest.event == 6,]
      }
      if (input$selection == "mis") {
        data <- data[data$Severest.event == 7,]
      }
      if (input$selection == "oth") {
        data <- data[data$Severest.event == 8,]
      }
      data %>% select(Event.description)
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
      par(bg = "#353c42")
      wordcloud_rep(names(v), v, scale = c(4.0,1.0),
                    min.freq = input$freq, max.words = input$max,
                    colors = viridis::viridis_pal(option = "D")(6))
    }, height = 470, width = 500)
    
    output$help_text <- renderUI({
      partID <- partID()
      part_event <- data[[variable]][data[["ID_code"]] == partID]
      paste0("here:", partID)
    })
  })
}
    
## To be copied in the UI
# mod_wordcloud_ui("wordcloud_1")
    
## To be copied in the server
# mod_wordcloud_server("wordcloud_1")
