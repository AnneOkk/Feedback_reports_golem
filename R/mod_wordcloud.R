#' wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wordcloud_ui <- function(id, evdes_df){
  ns <- NS(id)
  var_options1 <- c("Financial difficulties" = "evdes_df$`1`$t1evdes",
                   "Conflicts with clients, stakeholders or colleagues" = "evdes_df$`2`$t1evdes",
                   "Conflicts between clients, stakeholders, or colleagues" = "evdes_df$`3`$t1evdes",
                   "Legal issues" = "evdes_df$`4`$t1evdes",
                   "Absence or a lack of personnel or support" = "evdes_df$`5`$t1evdes",
                   "Problems related to material/ service supply or quality" = "evdes_df$`6`$t1evdes",
                   "Mistakes or mishaps" = "evdes_df$`7`$t1evdes",
                   "Other" = "`8`$t1evdes")
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
                min = 1,  max = 50, value = 15),
    sliderInput(ns("max"),
                "Maximum Number of Words:",
                min = 1,  max = 300,  value = 100),
    plotOutput(ns("plot"))
    )
  }
    
#' wordcloud Server Functions
#'
#' @noRd 
mod_wordcloud_server <- function(id, evdes_df){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Define a reactive expression for the document term matrix
    terms <- reactive({
      # Change when the "update" button is pressed...
      input$update
      # ...but not for anything else
      isolate({
        withProgress({
          setProgress(message = "Processing corpus...")
          getTermMatrix(input$selection)
        })
      })
    })
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale = c(4,0.5),
                    min.freq = input$freq, max.words = input$max,
                    colors = brewer.pal(8, "Dark2"))
    })
  })
}
    
## To be copied in the UI
# mod_wordcloud_ui("wordcloud_1")
    
## To be copied in the server
# mod_wordcloud_server("wordcloud_1")
