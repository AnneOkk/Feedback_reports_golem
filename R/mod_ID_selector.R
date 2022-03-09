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
    shinyFeedback::useShinyFeedback(),
    numericInput(ns("select"), 
               label = "Participant ID", 
               value = "",
               min = 1001, max = 1140)
  )

}



#' ID_selector Server Functions
#'
#' @noRd 
mod_ID_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    partID <- reactive({
      valid = input$select > 1000 & input$select < 1140
      shinyFeedback::feedbackWarning("select", !valid, "Enter value between 1001 and 1140", color = "#21918c ")
      input$select
    })
  })
}
    
## To be copied in the UI
# mod_ID_selector_ui("ID_selector_ui_1")
    
## To be copied in the server
# mod_ID_selector_server("ID_selector_ui_1")
