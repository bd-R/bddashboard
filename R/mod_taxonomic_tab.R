#' taxonomic_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_taxonomic_tab_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        6,
        mod_plotly_bars_ui(ns("plotly_bars_ui_1"))
      ),
      column(
        6,
        mod_plotly_bars_ui(ns("plotly_bars_ui_2"))
      )
    ),
    fluidRow(
      mod_DT_ui(ns("DT_ui_1"))
    )
  )
}
    
#' taxonomic_tab Server Function
#'
#' @noRd 
mod_taxonomic_tab_server <- function(input, output, session, data){
  ns <- session$ns
  
  data_reactive <- reactiveValues(data = data.frame(), events = list())
  
  observe({
    data_reactive$data = data()
  })
  
  callModule(mod_plotly_bars_server, "plotly_bars_ui_1", data_reactive,  data(), "scientificName", orientation ="h")
  callModule(mod_plotly_bars_server, "plotly_bars_ui_2", data_reactive,  data(), "countryCode", orientation ="h")
  
  callModule(mod_DT_server, "DT_ui_1", data_reactive, c(
    "countryCode",
    "locality",
    "decimalLatitude"
  ))
  
}
    
## To be copied in the UI
# mod_taxonomic_tab_ui("taxonomic_tab_ui_1")
    
## To be copied in the server
# callModule(mod_taxonomic_tab_server, "taxonomic_tab_ui_1")
 
