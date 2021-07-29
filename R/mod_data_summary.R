#' data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_summary_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      style = 'padding-top:-50px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_a"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_b"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_c"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton(ns("patients"), 
                   label = "Spatial", 
                   icon = icon("user"), 
                   style = "success bsButton"),
          bsButton(ns("antimicrobials"), 
                   label = "Temporal", 
                   icon = icon("spinner", class = "spinner-box"), 
                   style = "success bsButton"),
          bsButton(ns("diagnostics"), 
                   label = "Taxonomic", 
                   icon = icon("flask", class = "flask-box"), 
                   style = "success bsButton")
        )
      )
    ),

    
    fluidRow(
      style = 'padding-top:30px;',
      div(
        id = ns("patients_panel"),
        mod_leaflet_ui(ns("leaflet_ui_1"))
      )
    ),
    fluidRow(
      id = ns("spatial_value_box"),
      style = 'padding-top:30px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("map_coordinates"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("map_countries"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("map_locality"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      id = ns("temporal_line"),
      style = 'padding-top:30px;',
      column(
        12,
        mod_plotly_line_ui(ns("plotly_line_ui_1"))
      )
    ),
    fluidRow(
      id = ns("temporal_value_box"),
      style = 'padding-top:30px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("temporal_year"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("temporal_month"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("temporal_day"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      style = 'padding-top:30px;',
      div(
        id = ns("taxonomic_bar"),
        mod_plotly_bars_ui(ns("plotly_bars_ui_1"))
      )
    ),
    fluidRow(
      id = ns("taxonomic_value_box"),
      style = 'padding-top:30px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("taxonomic_scientificName"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("taxonomic_kingdom"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("taxonomic_family"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      div(
        id = ns("summary_data_table_id"),
        mod_DT_ui(ns("DT_ui_1"))
      )
    )
  )
}
    
#' data_summary Server Function
#'
#' @noRd 
mod_data_summary_server <- function(input, output, session, dataset){
  ns <- session$ns
  
  data_reactive <- reactiveValues(data = data.frame(), events = list(), leaflet_data=NULL)

  observe({
    dat <- dataset()
    data_reactive$data = dat
  })

  
  output$box5 <- renderText("Taxonomic")
  
  output$box_a <- shinydashboard::renderValueBox({
    
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    shinydashboard::valueBox(
      value = nrow(dataset()),
      subtitle = "# of Records",
      icon = icon("compass"),
      color = "aqua",
      width = 1
    )
  })
  
  output$box_b <-  shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('name' %in% colnames(dataset()), 'No appropriate Column found')
    )
    shinydashboard::valueBox(
      value = nrow(unique(dataset()["name"])),
      subtitle = "# of Taxa",
      icon = icon("file-signature"),
      color = "blue",
      width = 1
    )
  })
  
  output$box_c <-  shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    shinydashboard::valueBox(
      value = length(dataset()),
      subtitle = "# of Attributes",
      icon = icon("area-chart"),
      color = "light-blue",
      width = 1
    )
  })
  
  
  
  
  
  
  
  
  
  # hide the underlying selectInput in sidebar for better design
  
  observeEvent("", {
    shinyjs::hide("patients_panel")
    shinyjs::hide("spatial_value_box")
    shinyjs::hide("antimicrobials_panel")
    shinyjs::hide("taxonomic_bar")
    shinyjs::hide("taxonomic_value_box")
    shinyjs::hide("temporal_line")
    shinyjs::hide("temporal_value_box")
  }, once = TRUE)
  
  observeEvent(input$patients, {
    shinyjs::show("patients_panel")
    shinyjs::show("spatial_value_box")
    shinyjs::hide("antimicrobials_panel")
    shinyjs::hide("taxonomic_bar")
    shinyjs::hide("taxonomic_value_box")
    shinyjs::hide("temporal_line")
    shinyjs::hide("temporal_value_box")
  })
  observeEvent(input$antimicrobials, {
    shinyjs::show("antimicrobials_panel")
    shinyjs::show("temporal_line")
    shinyjs::show("temporal_value_box")
    shinyjs::hide("taxonomic_bar")
    shinyjs::hide("taxonomic_value_box")
    shinyjs::hide("patients_panel")
    shinyjs::hide("spatial_value_box")
  })
  observeEvent(input$diagnostics, {
    shinyjs::show("taxonomic_bar")
    shinyjs::show("taxonomic_value_box")
    shinyjs::hide("antimicrobials_panel")
    shinyjs::hide("patients_panel")
    shinyjs::hide("spatial_value_box")
    shinyjs::hide("temporal_line")
    shinyjs::hide("temporal_value_box")
  })

  
  
  callModule(mod_leaflet_server, "leaflet_ui_1", data_reactive,  dataset)

  output$map_coordinates <- shinydashboard::renderValueBox({
    dat <- dataset()
    if("verbatimLatitude" %in% colnames(dat))
    {
      latitudeName <- "verbatimLatitude"
    }else {
      latitudeName <- "decimalLatitude"
    }
    
    if("verbatimLongitude" %in% colnames(dat))
    {
      longitudeName <- "verbatimLongitude"
    }else {
      longitudeName <- "decimalLatitude"
    }
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    latitude <- nrow(
      (
        na.omit(
          dataset()[latitudeName]
        )
      )
    )
    
    longitude <- nrow(
      (
        na.omit(
          dataset()[longitudeName]
        )
      )
    )
    
    shinydashboard::valueBox(
      
      if(latitude>longitude){
        value = longitude
      } else {
        value = latitude
      },
      subtitle = "# of Geo Coordinates",
      icon = icon("compass"),
      color = "navy",
      width = 4
    )
  })
  
  output$map_countries <- shinydashboard::renderValueBox({
    df <- dataset()
    
    country_code_column_name <- 'countryCode'
    if('place_guess' %in% colnames(df)){
      country_code_column_name <- 'place_guess'
    } else if('calculatedCountry' %in% colnames(df)){
      country_code_column_name <- 'calculatedCountry'
    } else if('country' %in% colnames(df)){
      country_code_column_name <- 'country'
    } else if('country' %in% colnames(df)){
      country_code_column_name <- 'country'
    } 
    
    validate(
      need(length(df)>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need(country_code_column_name %in% colnames(df), 'No appropriate Column found with country names in it.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()[country_code_column_name]
          )
        )
      ),
      subtitle = "# of Countries",
      icon = icon("copyright"),
      color = "navy",
      width = 4
    )
  })
  
  output$map_locality <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('locality' %in% colnames(dataset()), 'No appropriate Column found with locality data.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()["locality"]
          )
        )
      ),
      subtitle = "# of Localities",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  

  #Temporal
  callModule(mod_plotly_line_server, "plotly_line_ui_1", data_reactive,  dataset, "kingdom", "year", "daily")
  
  output$temporal_year <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('year' %in% colnames(dataset()), 'No appropriate Column found with year data.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()["year"]
          )
        )
      ),
      subtitle = "# of Years",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  
  output$temporal_month <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('month' %in% colnames(dataset()), 'No appropriate Column found with Month data.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()["month"]
          )
        )
      ),
      subtitle = "# of Months",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  
  
  output$temporal_day <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('day' %in% colnames(dataset()), 'No appropriate Column found with Day data.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()["day"]
          )
        )
      ),
      subtitle = "# of Days",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  
  
  #Taxonomic
  callModule(mod_plotly_bars_server, "plotly_bars_ui_1", data_reactive,  dataset, "genus", orientation ="h")
  
  
  output$taxonomic_scientificName <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('scientificName' %in% colnames(dataset()), 'No appropriate Column found with scientificName data.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()["scientificName"]
          )
        )
      ),
      subtitle = "# of Scientific Name",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  
  output$taxonomic_kingdom <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('kingdom' %in% colnames(dataset()), 'No appropriate Column found with kingdom data.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()["kingdom"]
          )
        )
      ),
      subtitle = "# of Kingdom",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  
  output$taxonomic_family <- shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('family' %in% colnames(dataset()), 'No appropriate Column found with family data.')
    )
    
    shinydashboard::valueBox(
      value = nrow(
        unique(
          na.omit(
            dataset()["family"]
          )
        )
      ),
      subtitle = "# of Family",
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
  

  
  
  
  
  callModule(mod_DT_server, "DT_ui_1", data_reactive, c(
    "countryCode",
    "locality",
    "decimalLatitude"
  ))
  
 
}
    
## To be copied in the UI
# mod_data_summary_ui("data_summary_ui_1")
    
## To be copied in the server
# callModule(mod_data_summary_server, "data_summary_ui_1")
 
