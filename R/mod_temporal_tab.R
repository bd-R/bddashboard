#' temporal_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_temporal_tab_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        6,
        mod_plotly_bars_ui(ns("plotly_bars_ui_1"))
      ),
      column(
        6,
        mod_plotly_bubble_ui(ns("plotly_bubble_ui_1"))
      )
    ),
    fluidRow(
      mod_plotly_bubble_ui(ns("plotly_line_ui_1"))
    )
  )
}
    
#' temporal_tab Server Function
#'
#' @noRd 
mod_temporal_tab_server <- function(input, output, session, data){
  ns <- session$ns
  
  data_reactive <- reactiveValues(data = data.frame(), events = list())
  
  observe({
    data_reactive$data = data()
  })
  
  
  callModule(mod_plotly_bars_server, "plotly_bars_ui_1", data_reactive,  data(), "typeStatus", orientation ="v")
  callModule(mod_plotly_bubble_server, "plotly_bubble_ui_1", data_reactive,  data(), "species", "year")
  callModule(mod_plotly_line_server, "plotly_line_ui_1", data_reactive,  data(), "species", "year", "cumulative")
  
  
 
}
    
## To be copied in the UI
# mod_temporal_tab_ui("temporal_tab_ui_1")
    
## To be copied in the server
# callModule(mod_temporal_tab_server, "temporal_tab_ui_1")
 
