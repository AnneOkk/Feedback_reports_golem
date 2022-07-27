#' wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_wordcloud_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' wordcloud Server Functions
#'
#' @noRd 
mod_wordcloud_server <- function(id){
  moduleServer( id, function(input, output, session){
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
