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
    div(style="display:inline-block", # inline block to show selections side by side instead of above each other
        selectInput(ns("select_var1"), 
                    # second select variable for size (continuous)
                    label = "Select variable to plot:", 
                    choices =  c("Age" = "Age",
                                "Job satisfaction" = "t1jobsa",
                                "Job strain" = "t1jobstr",
                                "Average event severity" = "Mean_event_severity"),
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
    ranges <- reactiveValues(x = c(min(df[, "LocationLon"], na.rm = T), max(df[, "LocationLon"], na.rm = T)), 
                             y = c(min(df[, "LocationLat"], na.rm = T), max(df[, "LocationLat"], na.rm = T))) # set initial ranges for the world map
    size <- reactiveValues(height = 300, width = 900) # set initial plot height and width (no clicking)
    
    selected_var1 <- reactive(input$select_var1) # first selected variable
    
    # assign breaks and labels for selected variable 
    breaks <- reactive(if(selected_var1() == "Age"){
      c(20, 30, 40, 50, 60, 70, 80, 90)
    } else {
      quantile(df[, selected_var1()], prob=c(0.2, 0.4, 0.6, 0.8, 1.0), na.rm = T)
    })
    
    labels <- reactive(if(selected_var1() == "Age"){
      c(20, 30, 40, 50, 60, 70, 80, 90)
    }else if (selected_var1() == "t1jobsa") {
      c("extremely dissatisfied", "somewhat dissatisfied", "neither satisfied nor dissatisfied", "somewhat satisfied", "extremely satisfied")
    }else if (selected_var1() == "t1jobstr") {
      c("never feeling strained", "sometimes feeling strained", "about half of the time feeling strained", "most of the time feeling strained", "always feeling strained")
    }else if (selected_var1() == "Mean_event_severity") {
      c("not at all severe", "a little severe", "somewhat severe", "very severe", "extremely severe")
    })
      
    # make plot
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
                        ,fill = df[, selected_var1()]
                        ,size = df[, selected_var1()]
                        ),
          alpha = 0.7, 
          shape = 21) + 
        scale_fill_viridis_c(guide = "legend") +
        scale_size_continuous(range = c(1, 12)) + 
        # set limits for the world map latitude and longitude
        scale_x_continuous(limits = ranges$x) + 
        scale_y_continuous(limits = ranges$y) +
        scale_fill_continuous(breaks = breaks(),
                               labels = labels()) +
        theme_void() + # remove background
        labs(fill = colnames(df[, selected_var1()]), 
             size = colnames(df[, selected_var1()])) 
    }, 
    bg="transparent", 
    height = reactive(size$height), 
    width = reactive(size$width)
    )
    
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
    observeEvent(input$plot1_dblclick, {
      size$height = 300 # adjust map size based on double click 
      size$width  = 500
    })
    })
}
        
        
    
## To be copied in the UI
# mod_world_map_ui("world_map_ui_1")
    
## To be copied in the server
# mod_world_map_server("world_map_ui_1")
