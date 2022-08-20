#' relations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_relations_ui <- function(id){
  plotOutput(NS(id, "reg_plot")
  )
}
    
#' relations Server Functions
#'
#' @noRd 
mod_relations_server <- function(id, df, partID){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$reg_plot <- renderPlot({
      partID <- partID()
      regplot(data = df, x = "t1meansev", y = "t1jobsa", 
              partID)
    }
    )
  })
}
    
## To be copied in the UI
# mod_relations_ui("relations_1")
    
## To be copied in the server
# mod_relations_server("relations_1")
