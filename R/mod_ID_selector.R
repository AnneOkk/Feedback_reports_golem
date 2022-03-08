#' ID_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shinyFeedback
#'
#' @importFrom shiny NS tagList 
mod_ID_selector_ui <- function(id){
  ns <- NS(id)
  shiny::tagList(
    useShinyFeedback(),
    numericInput(ns("select"), 
               label = "Please enter your Participant ID", 
               value = 1000,
               min = 1001, max = 1140)
  )

}



#' ID_selector Server Functions
#'
#' @noRd 
mod_ID_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$select, {
      if (input$select > 1140 | input$select < 1001) {
        showFeedbackWarning(
          inputId = "select",
          text = "Enter ID between 1001 and 1140"
        )  
      } else {
        hideFeedback("select")
      }
    })
    partID <- reactive({
      if (is.null(input$select)){
        return(1000)
      } else {
        return(input$select)
      }
    })
  })
}
    
## To be copied in the UI
# mod_ID_selector_ui("ID_selector_ui_1")
    
## To be copied in the server
# mod_ID_selector_server("ID_selector_ui_1")
