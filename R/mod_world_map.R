#' world_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2 sf

mod_world_map_ui <- function(id, df){
  ns <- NS(id)
  shiny::tagList(
    div(style="display:inline-block", selectInput(ns("select_var1"), 
                                                  label = "Select variable to plot:", 
                                                  choices =  c("Age" = "Age",
                                                               "Gender" = "Gender",
                                                               "Educational level" ="Education_level",
                                                               "Industry" = "Industry",
                                                               "Job satisfaction" = "t1jobsa",
                                                               "Job strain" = "t1jobstr",
                                                               "Average event severity" = "Mean_event_severity",
                                                               "Severest event" = "Severest_event"),
                                                  selected = NULL)),
    div(style="display:inline-block", selectInput(ns("select_var2"), 
                   label = "Select variable to plot:", 
                   choices =  c("Age" = "Age",
                                "Gender" = "Gender",
                                "Educational level" ="Education_level",
                                "Industry" = "Industry",
                                "Job satisfaction" = "t1jobsa",
                                "Job strain" = "t1jobstr",
                                "Average event severity" = "Mean_event_severity",
                                "Severest event" = "Severest_event"),
                   selected = NULL)),
    plotOutput(NS(id, "plot"), 
               dblclick = ns("plot1_dblclick"),
               brush = brushOpts(
                 id = ns("plot1_brush"),
                 resetOnNew = TRUE,
                 fill = "#FDE725FF",
               )),
    downloadButton('downloadplot', label = 'Download Plot'),
    actionButton("reset", "Default colours", icon = icon("undo"))
  )
}
    
#' world_map Server Functions
#'
#' @noRd 
mod_world_map_server <- function(id, df, world_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ranges <- reactiveValues(x = NULL, y = NULL)
    selected_var <- reactive(input$select_var)
    output$plot <- renderPlot({
      ggplot2::ggplot(data = world_df) +
        ggplot2::geom_map(
          map = world_df,
          aes(long, lat, map_id = region),
          fill = "#e9ecef"
        ) +
        ggplot2::geom_point(
          data = df,
          mapping = aes(LocationLon, LocationLat
                        #, color = df[, selected_var()]
                        ),
          alpha = 0.7) + 
        scale_x_continuous(limits = ranges$x) +
        scale_y_continuous(limits = ranges$y) +
        theme_void()
    }, bg="transparent")
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- c(min(df[, "LocationLon"], na.rm = T), max(df[, "LocationLon"], na.rm = T))
        ranges$y <- c(min(df[, "LocationLat"], na.rm = T), max(df[, "LocationLat"], na.rm = T))
      }
    })
    })
}
        
        
    
## To be copied in the UI
# mod_world_map_ui("world_map_ui_1")
    
## To be copied in the server
# mod_world_map_server("world_map_ui_1")
