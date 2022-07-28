#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd

app_server <- function(input, output, session) {
  observeEvent(input$reload, {
    session$reload()
    })
  #part_id <- reactive(input$Participant)
  #df <- golex::df
  #part_id_age = reactive(df[df$ID_code == input$Participant, ] %>% dplyr::select(dplyr::matches("t1age_1")) %>% .[1, 1]),
  
  selectedID <- mod_ID_selector_server("ID_selector_ui_1")
  mod_age_hist_server("age_hist_ui_1", df, partID = selectedID)
  mod_gender_plot_server("gender_plot_ui_1", df)
  mod_sample_info_server("sample_info_ui_1", df)
  mod_world_map_server("world_map_ui_1", df, world_df)
  mod_wordcloud_server("wordcloud_1", df, partID = selectedID)
}
