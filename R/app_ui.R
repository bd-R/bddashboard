#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinydashboard dashboardthemes shinyjs bdutilities.app
#' @import plotly DT leaflet leaflet.extras sp shinyWidgets dplyr 
#' @import promises future RColorBrewer readr
#' @import rintrojs shinyBS flexdashboard formattable
#' @noRd

future::plan(future::sequential())

app_ui <- function(request) {
  dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "bddashboard Experiment"),
    dashboardSidebar(
      sidebarMenu(
        id = "sideBar",
        menuItem(
          "Data Input",
          tabName = "dataInputTab",
          icon = icon("database")
        ),
        menuItem(
          "Data Summary",
          tabName = "dataSummary",
          icon = icon("eye")
        ),
        menuItem(
          "Missing Data",
          tabName = "missing_overview",
          icon = icon("database")
        ),
        menuItem(
          "Spatial",
          tabName = "spatial_tab",
          icon = icon("eye")
        ),
        menuItem(
          "Taxonomic",
          tabName = "taxonomic_tab",
          icon = icon("eye")
        ),
        menuItem(
          "Temporal",
          tabName = "temporal_tab",
          icon = icon("eye")
        )
      )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      golem_add_external_resources(),
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "dataInputTab",
          bdutilities.app::mod_add_data_ui("bdFileInput"),
          bdutilities.app::mod_darwinize_ui("darwinize")
        ),
        tabItem(
          tabName = "dataSummary",
          mod_data_summary_ui("data_summary_ui_1")
        ),
        tabItem(
          tabName = "missing_overview",
          mod_missing_data_ui("missing_data_ui_1")
        ),
        tabItem(
          tabName = "spatial_tab",
          mod_spatial_tab_ui("spatial_tab_ui_1")
        ),
        tabItem(
          tabName = "taxonomic_tab",
          mod_taxonomic_tab_ui("taxonomic_tab_ui_1")
        ),
        tabItem(
          tabName = "temporal_tab",
          mod_temporal_tab_ui("temporal_tab_ui_1")
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
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'bddashboard'
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

