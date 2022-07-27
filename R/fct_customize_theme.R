#' Customizes the theme
#'
#' @description Creates a customizes theme with the help of the fresh package
#'
#' @import fresh
customize_theme <- function() {
  create_theme(
    bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFFFFF",
      navbar_light_hover_color = "#FFFFFF"
    ),
    bs4dash_layout(
      main_bg = "#353c42"
    ),
    bs4dash_sidebar_light(
      bg = "#f6f6f6",
      color = "#676767",
      hover_color = "#FFFFFF",
      submenu_bg = "#272c30",
      submenu_color = "#FFFFFF",
      submenu_hover_color = "#FFFFFF"
    ),
    bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#f6f6f6"
    ),
    bs4dash_color(
      gray_900 = "#FFFFFF", white = "#272c30"
    )
  )}
