#' world_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import ggplot2 sf sjlabelled


mod_world_map_ui <- function(id, df){
  ns <- NS(id)
  #categorical variable selection
  var_options1 <- c("Age" = "Age",
                   "Education level" = "Education.level",
                   "Co-ownership" = "Co.ownership",
                   "Industry" = "Industry",
                   "Job satisfaction" = "Job.satisfaction",
                   "Job strain" = "Job.strain"
                    )
  shiny::tagList(
    div(style = "display:inline-block", # inline block to show selections side by side instead of above each other
        selectInput(ns("select_var1"), 
                    # second select variable for size (continuous)
                    label = "Select variable to plot:", 
                    choices =  var_options1,
                    selected = var_options1[1])
        ),
  plotOutput(ns("plot"), 
               click = ns("plot1_click"),
               dblclick = ns("plot1_dblclick"),
               brush = brushOpts(
                 id = ns("plot1_brush"),
                 resetOnNew = TRUE,
                 fill = "#FDE725FF")
               ),
  downloadButton('downloadplot', label = 'Download Plot')
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
    size <- reactiveValues(height = 340, width = 800) # set initial plot height and width (no clicking)
    
    selected_var1 <- reactive(input$select_var1) # selected variable

        
    output$plot <- renderPlot({
      df <- sjlabelled::remove_all_labels(df) # remove all labels (labelled cannot be handled by ggplot?)
      # assign breaks and labels for selected variable 
      breaks <- if(selected_var1() == "Age"){
        c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
      } else if(selected_var1() == "Education.level"){
        c(2, 3, 4, 5, 6)
      } else if(selected_var1() == "Co.ownership"){
        c(1, 2)
      } else if(selected_var1() == "Industry"){
        c(1, 2, 3, 4, 5, 6, 7)
      } else if (selected_var1() == "Job.satisfaction"){
        c(1, 2, 3, 4, 5)
      } else if(selected_var1() == "Job.strain"){
        c(1, 2, 3, 4, 5)
      }

      labels <- if(selected_var1() == "Age"){
        c("20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70")
      } else if(selected_var1() == "Education.level"){
        c("Secondary school", "Technical school diploma", "University degree", "Doctorate degree", "Other")
      } else if(selected_var1() == "Co.ownership"){
        c("Single owner", "Co-owned venture")
      } else if(selected_var1() == "Industry"){
        c("Information, Technology", 
          "Finance, Real Estate", 
          "Social and Consumer Services", 
          "Wholesale, Retail",
          "Manufacturing, Logistics",
          "Agriculture, Construction",
          "Other")
      } else if(selected_var1() == "Job.satisfaction"){
        c("extremely dissatisfied", 
          "somewhat dissatisfied",
          "neither satisfied \nnor dissatisfied",
          "somewhat satisfied",                  
          "extremely satisfied")
      } else if(selected_var1() == "Job.strain"){
        c("never feeling strained",
          "sometimes feeling strained",
          "about half of the \ntime feeling strained",
          "most of the time \nfeeling strained",
          "always feeling strained")
      }
      
      range <- if(selected_var1() == "Education.level" || selected_var1() == "Co.ownership" || selected_var1() == "Industry"){
        c(6, 6) 
      } else {
        c(4, 13)
      }
      
      
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
        guides(color = guide_legend(override.aes = list(size = 9))) +
        scale_fill_viridis_c(guide = "legend", breaks = breaks, labels = labels) +
        scale_size_continuous(range = range, breaks = breaks, labels = labels) + 
        # set limits for the world map latitude and longitude
        scale_x_continuous(limits = ranges$x) + 
        scale_y_continuous(limits = ranges$y) +
        theme_void() + # remove background
        labs(fill = selected_var1(), 
             size = selected_var1()) +
        theme(legend.text = element_text(size=12, color = "white")) + 
        theme(legend.title = element_text(size=14, color = "white")) + 
        theme(legend.title.align = 0.5) +
        theme(legend.key.size = unit(1.1, "cm"))  # increases vertical space between legend items
    }, 
    bg="transparent", 
    height = reactive(size$height), 
    width = reactive(size$width)
    )
    
    observeEvent(input$plot1_brush, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        size$height = 400 # scale map based on brush 
        size$width  = 700
      } else {
        ranges$x <- c(min(df[, "LocationLon"], na.rm = T), max(df[, "LocationLon"], na.rm = T))
        ranges$y <- c(min(df[, "LocationLat"], na.rm = T), max(df[, "LocationLat"], na.rm = T))
      }
    })
    observeEvent(input$clearBrush, {
      session$resetBrush("plotBrush")
    })
    
    observeEvent(input$resetPlot, {
      session$resetBrush("plotBrush")
      brush <<- NULL
    })

    observeEvent(input$plot1_dblclick, {
      ranges$x <- c(min(df[, "LocationLon"], na.rm = T), max(df[, "LocationLon"], na.rm = T))
      ranges$y <- c(min(df[, "LocationLat"], na.rm = T), max(df[, "LocationLat"], na.rm = T))
      size$height = 400 # rescale map back to original based on double click 
      size$width  = 1000
    })
    })
}
        
        
    
## To be copied in the UI
# mod_world_map_ui("world_map_ui_1")
    
## To be copied in the server
# mod_world_map_server("world_map_ui_1")
