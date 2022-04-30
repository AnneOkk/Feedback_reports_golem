#' xgboost UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_xgboost_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' xgboost Server Functions
#'
#' @noRd 
mod_xgboost_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_xgboost_ui("xgboost_ui_1")
    
## To be copied in the server
# mod_xgboost_server("xgboost_ui_1")
