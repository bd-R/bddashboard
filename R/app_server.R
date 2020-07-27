#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  #Increase Upload limit
  options(shiny.maxRequestSize = 5000 * 1024 ^ 2)
  
  
  data_store <-
    shiny::reactiveValues(
      darwinized_data = data.frame(),
      input_data = data.frame()
    )
  
  data_store$input_data <-
    callModule(
      bdutilities.app::mod_add_data_server,
      id = "bdFileInput",
      "darwinControlInner"
    )
  
  data_store$darwinized_data <-
    callModule(bdutilities.app::mod_darwinize_server,
               "darwinize",
               dat = data_store$input_data)

}
