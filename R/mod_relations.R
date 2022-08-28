#' relations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import ggplot2 huxtable DT
#' @importFrom shiny NS tagList 

mod_relations_ui <- function(id){
  ns <- NS(id)
  #categorical variable selection
  var_options1 <- c("Job satisfaction" = "t1jobsa",
                    "Job strain" = "t1jobstr")
  shiny::tagList(
    div(style = "display:inline-block", # inline block to show selections side by side instead of above each other
        selectInput(ns("select_var"), 
                    # second select variable for size (continuous)
                    label = "Select variable to plot:", 
                    choices =  var_options1,
                    selected = var_options1[1])
    ),
    shiny::fluidRow(
      column(
        width = 7, 
        plotOutput(NS(id, "reg_plot"))
      ),
      column(
        width = 3,
        uiOutput(NS(id, "reg_table"))
      )
  )
  )
}
    
#' relations Server Functions
#'
#' @noRd 
mod_relations_server <- function(id, df, partID){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    selected_var <- reactive(input$select_var)
    output$reg_plot <- renderPlot({
      partID <- partID()
      regplot(data = df, x = "t1meansev", y = selected_var(), partID = partID, xname = "Mean Error Severity", yname = selected_var())
    }, bg = "transparent")
    output$reg_table <- renderUI({
      HTML(huxtable::to_html(regmodel(data = df, x = "t1meansev", y = selected_var(), controls = c("t1gender", "Age"), coef_names = c("Mean error severity" = "t1meansev", "Gender" = "t1gender", "Age" = "Age"), model_name = "Regression Model")))
      })
  })
}
    
## To be copied in the UI
# mod_relations_ui("relations_1")
    
## To be copied in the server
# mod_relations_server("relations_1")
