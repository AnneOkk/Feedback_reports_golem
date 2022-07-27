#' The application User-Interface
#' @title Feedback Reports UI
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardBrand dashboardSidebar sidebarMenu sidebarHeader menuItem dashboardBody tabItems tabItem
#' @import waiter fresh shiny


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    bs4Dash::dashboardPage(
      title = "Entrepreneur Feedback Reports",
      fullscreen = FALSE,
      help = FALSE,
      preloader = list(html = tagList(waiter::spin_solar(), "Loading ..."), color = "#21918c"),
      dark = TRUE,
      freshTheme = customize_theme(),
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "Feedback Reports",
          href = "https://github.com/AnneOkk",
          image = "https://s7.gifyu.com/images/chat-1873543_1280.png",
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        id = "sidebar",
        brandColor = "primary",
        collapsed = TRUE,
        bs4Dash::sidebarMenu(
          id = "welcome",
          bs4Dash::sidebarHeader("Welcome!"),
          bs4Dash::menuItem(
            text = "Welcome",
            tabName = "welcome",
            icon = icon("star")
          )
        ),
        bs4Dash::sidebarMenu(
          id = "sample_info",
          bs4Dash::menuItem(
            text = "Sample information",
            tabName = "info",
            icon = icon("user-friends")
          )
        ),
        bs4Dash::sidebarMenu(
          id = "events",
          bs4Dash::menuItem(
            text = "Event texts",
            tabName = "event",
            icon = icon("user-friends")
          )
        ),
        bs4Dash::sidebarMenu(
          id = "part_id",
          bs4Dash::menuItem(
            mod_ID_selector_ui("ID_selector_ui_1"),
            text = "Participant ID",
            icon = icon("sliders-h")
          )
        )
      ),
        body = bs4Dash::dashboardBody(
          bs4Dash::tabItems(
            bs4Dash::tabItem(
              tabName = "welcome",
              fluidRow(
                column(12,
                       align = "center",
                       h1("Feedback Reports"),
                       h2("Dealing with entrepreneurial work events"),
                       actionButton("reload", "Reload")
                )
              )
            ),
            bs4Dash::tabItem(
              tabName = "info",
              fluidRow(
                column(
                  offset = 0.2,
                  width = 11, title = "Participants",
                  height = "20px",
                  mod_sample_info_ui("sample_info_ui_1"),
                  br()
                  )
                ),
              fluidRow(
                column(
                  4, 
                  height = "70px",
                  h5(style="text-align: center;","Sample age"),
                  mod_age_hist_ui("age_hist_ui_1"),
                  icon("question-circle"),
                  age_text
                ), 
                column(
                  4, 
                  height = "70px",
                  h5(style="text-align: center;","Sample gender"),
                  mod_gender_plot_ui("gender_plot_ui_1"),
                  icon("question-circle"),
                  gender_text
                )
              ),
              fluidRow(
                column(
                  8, 
                  height = "90px",
                  h5(style="text-align: center;","World data"),
                  mod_world_map_ui("world_map_ui_1", df)
                )
            )
          ),
          bs4Dash::tabItem(
            tabName = "event",
            fluidRow(
              column(
                offset = 0.2,
                width = 11, title = "Events",
                height = "20px",
                mod_wordcloud_ui("wordcloud_1"),
                br()
              )
            )
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'golex'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}



