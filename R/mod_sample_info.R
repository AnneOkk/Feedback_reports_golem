#' sample_info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_sample_info_ui <- function(id){
    h4(htmlOutput(NS(id, "sample_N"), style = "color: white",
       background = NULL)
    )
}

    
#' sample_info Server Functions
#'
#' @noRd 
mod_sample_info_server <- function(id, df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$sample_N <- renderText({
      paste("Sample size: <em>N</em> = ",
            length(unique(df$ID_code)),
            sep = ""
      )
    })
  })
}
    
## To be copied in the UI
# mod_sample_info_ui("sample_info_ui_1")
    
## To be copied in the server
# mod_sample_info_server("sample_info_ui_1")
