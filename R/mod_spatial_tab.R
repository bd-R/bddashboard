#' spatial_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spatial_tab_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      mod_leaflet_ui(ns("leaflet_ui_1"))
    ),
    fluidRow(
      mod_DT_ui(ns("DT_ui_1"))
    ),
    fluidRow(
      id="data_rows",
      br(),
      uiOutput(ns("data_rows_ui"))
    )
  )
}
    
#' spatial_tab Server Function
#'
#' @noRd 
mod_spatial_tab_server <- function(input, output, session, data){
  ns <- session$ns
  

  data_reactive <- reactiveValues(data = data.frame(), events = list(), leaflet_data=NULL)

  observe({
    data_reactive$data = data()
  })
  
  output$data_rows_ui <- renderUI({
    verbatimTextOutput(ns("data_length"))
  })
  
  output$data_length <- renderText({
    total_length <- nrow(data())
    current_length <- nrow(data_reactive$data)
    paste0("Showing ",current_length,"/",total_length, " rows")
  })

  
  
  callModule(mod_leaflet_server, "leaflet_ui_1", data_reactive,  data)
  callModule(mod_DT_server, "DT_ui_1", data_reactive, c(
    "countryCode",
    "locality",
    "decimalLatitude"
  ))
  
 
}
    
## To be copied in the UI
# mod_spatial_tab_ui("spatial_tab_ui_1")
    
## To be copied in the server
# callModule(mod_spatial_tab_server, "spatial_tab_ui_1")
 