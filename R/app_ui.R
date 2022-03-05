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
          href = "https://felixbehne.shinyapps.io/ant-colony-optimization/",
          image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRaxf3S4xKkxZrXiIElsH4c1DQbHc79JdrF0A&usqp=CAU",
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
            icon = icon("calendar-alt")
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
                  height = "10px",
                  mod_sample_info_ui("sample_info_ui_1")
                  )
                ),
              fluidRow(
                column(
                  4, 
                  height = "70px",
                  mod_age_hist_ui("age_hist_ui_1"),
                  icon("question-circle"),
                  tags$div("Research published in the",
                           tags$a(href = "https://hbr.org/2018/07/research-the-average-age-of-a-successful-startup-founder-is-45#", 
                                  "Harvard Business Review (2018)"),
                                  "shows that the average age at which a successful founder started their company is 45. 
                                  According to the report,'that’s among the top 0.1% of startups based on growth in their first five years'."
                  )
                ), 
                column(
                  7, 
                  height = "70px",
                  mod_gender_plot_ui("gender_plot_ui_1"),
                  icon("question-circle"),
                  tags$div(
                    "According to data of the",
                    tags$a(
                      href = "https://blogs.worldbank.org/opendata/women-entrepreneurs-needed-stat",
                      "Gender Data Portal of the World Bank Group,"),
                    "1 in 3 businesses are owned by a woman."
                )
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



