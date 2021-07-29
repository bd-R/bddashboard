#' missing_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_missing_data_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      style = 'padding-bottom:0px;',
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("gauge_one")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("gauge_two")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("gauge_three")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("gauge_four")
        )
      )
    ),
    fluidRow(
      tabsetPanel(
        id = ns("first_tabset"),
        tabPanel(
          "Spatial",
          value = "spatial",
          formattable::formattableOutput(
            ns("spatial_table")
          )
        ),
        tabPanel(
          "Temporal",
          value = "temporal",
          formattable::formattableOutput(
            ns("temporal_table")
          )
        ),
        tabPanel(
          "Taxonomic",
          value = "taxonomic",
          formattable::formattableOutput(
            ns("taxonomic_table" )
          )
        )
      )
    ),
    tags$br(),
    tags$br(),
    h4("Missing Darwin Core Fields"),
    fluidRow(
      tabsetPanel(
        id = ns("second_tabset"),
        tabPanel(
          "Spatial",
          value = "spatial",
          formattable::formattableOutput(ns("spatial_missing"))
        ),
        tabPanel(
          "Temporal",
          value = "temporal",
          formattable::formattableOutput(ns("temporal_missing"))
        ),
        tabPanel(
          "Taxonomic",
          value = "taxonomic",
          formattable::formattableOutput(ns("taxonomic_missing"))
        )
      )
    )
  )
  
}
    
#' missing_data Server Function
#'
#' @noRd 
mod_missing_data_server <- function(input, output, session, dataset_missing){
  ns <- session$ns
  
  output$gauge_one <- flexdashboard::renderGauge({
    dat <- dataset_missing()
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
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    df <- dataset_missing()
    
    latitude <- round(
      (
        (
          nrow(
            df[latitudeName]) - sum(
              is.na(
                df[latitudeName]
              )
            )
        ) / nrow(
          df[latitudeName]
        )
      ),
      2
    ) * 100
    
    longitude <- round(
      (
        (
          nrow(
            df[latitudeName]) - sum(
              is.na(
                df[latitudeName]
              )
            )
        ) / nrow(
          df[latitudeName]
        )
      ),
      2
    ) * 100
    
    if (latitude > longitude) {
      geo <- longitude
    } else {
      geo <- latitude
    }
    
    gauge(
      geo,
      min = 0,
      max = 100,
      symbol = "%",
      label = "% of georeferenced \nrecords",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
  
  output$gauge_two <- flexdashboard::renderGauge({
    df <- dataset_missing()
    columnName <- 'year'
    if('dateModified' %in% colnames(df)){
      columnName <- 'dateModified'
    } else if('datecollected' %in% colnames(df)){
      columnName <- 'datecollected'
    } else if('begin_date' %in% colnames(df)){
      columnName <- 'begin_date'
    } else if('date' %in% colnames(df)){
      columnName <- 'date'
    } else if('observed_on' %in% colnames(df)){
      columnName <- 'observed_on'
    } 
    validate(
      need(length(df)>0, 'Please upload/download a dataset first')
    )
    validate(
      need(columnName %in% colnames(df), 'No appropriate Column with Date data present in Database!')
    )
    
    
    
    
    countryRecord <- round(
      (
        (
          nrow(
            df[columnName]) - sum(
              is.na(
                df[columnName]
              )
            )
        ) / nrow(
          df[columnName]
        )
      ),
      2
    ) * 100
    
    gauge(
      countryRecord,
      min = 0,
      max = 100,
      symbol = "%",
      label = "% of records\nwith date data",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
  
  output$gauge_three <- flexdashboard::renderGauge({
    df <- dataset_missing()
    occurance_column_name <- 'occurrenceID'
    if('uri' %in% colnames(df)){
      occurance_column_name <- 'uri'
    } else if ('remote_resource' %in% colnames(df)){
      occurance_column_name <- 'remote_resource'
    }
    
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need(occurance_column_name %in% colnames(df), 'No appropriate Column found with occurance remark data/link')
    )
    df <- dataset_missing()
    
    institutionCode <- round(
      (
        (
          nrow(
            df[occurance_column_name]) - sum(
              is.na(
                df[occurance_column_name]
              )
            )
        ) / nrow(
          df[occurance_column_name]
        )
      ),
      2
    ) * 100
    
    gauge(
      institutionCode,
      min = 0,
      max = 100,
      symbol = "%",
      label = "% of records\nwith occurence remark/link",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
  
  output$gauge_four <- flexdashboard::renderGauge({
    
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('basisOfRecord' %in% colnames(dataset_missing()), 'No appropriate Column found with basisOfRecord data')
    )
    df <- dataset_missing()
    
    basisOfRecord <- round(
      (
        (
          nrow(
            df["basisOfRecord"]) - sum(
              is.na(
                df["basisOfRecord"]
              )
            )
        ) / nrow(
          df["basisOfRecord"]
        )
      ),
      2
    ) * 100
    
    gauge(
      basisOfRecord,
      min = 0,
      max = 100,
      symbol = "%",
      label = "% of records\nwith basisOfRecord data",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
  
  #Calculating missing data and create the table for spatial Tab
  output$spatial_table <- formattable::renderFormattable({
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    df <- dataset_missing()
    missing_name <- vector()
    names <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    spatial_column <- c(
      "countryCode",
      "locality",
      "decimalLatitude",
      "decimalLongitude",
      "verbatimLatitude",
      "verbatimLongitude",
      "coordinateUncertaintyInMeters",
      "coordinatePrecision",
      "elevation",
      "elevationAccuracy",
      "depth",
      "depthAccuracy",
      "establishmentMeans"
    )
    
    for(i in spatial_column){
      if(i %in% colnames(df)){
        names <- c(names,i)
        total_records <- c(
          total_records,
          nrow(df[i])
        )
        missing_records <- c(
          missing_records,
          sum(
            is.na(
              df[i]
            )
          )
        )
        records_percentage <- c(
          records_percentage,
          round(
            (
              (
                nrow(
                  df[i]
                ) - sum(
                  is.na(
                    df[i]
                  )
                )
              ) /
                nrow(
                  df[i]
                )
            ),
            2
          ) * 100
        )
      }else {
        missing_name <- c(missing_name,i)
      }
    }
    
    
    output$spatial_missing <- formattable::renderFormattable({
      table <- data.frame(missing_name)
      formattable::formattable(
        table,
        align = c(
          "c",
          rep(
            "l",
            NCOL(
              table
            ) - 1
          )
        )
      )
    })
    
    table <- data.frame(
      names,
      total_records,
      missing_records,
      records_percentage
    )
    
    customRed <- "#ff7f7f"
    
    unit.scale = function(x){
      x/100
    }
    formattable::formattable(
      table,
      align = c(
        "l",
        rep(
          "r",
          NCOL(
            table
          ) - 1
        )
      ),
      list(
        records_percentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
  
  #Calculating missing data and create the table for Temporal Tab  
  output$temporal_table <- formattable::renderFormattable({
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    df <- dataset_missing()
    names <- vector()
    missing_name <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    temporal_column <-
      c(
        "eventDate",
        "day",
        "month",
        "year",
        "dateIdentified",
        "lastInterpreted",
        "dateModified",
        "datecollected",
        "begin_date",
        "observed_on",
        "date"
      )
    
    for(i in temporal_column){
      if(i %in% colnames(df)){
        names <- c(names,i)
        total_records <- c(
          total_records,
          nrow(df[i])
        )
        missing_records <- c(
          missing_records,
          sum(
            is.na(
              df[i]
            )
          )
        )
        records_percentage <- c(
          records_percentage,
          round(
            (
              (
                nrow(
                  df[i]
                ) - sum(
                  is.na(
                    df[i]
                  )
                )
              ) /
                nrow(
                  df[i]
                )
            ),
            2
          ) * 100
        )
      }else {
        missing_name <- c(missing_name,i)
      }
    }
    
    output$temporal_missing <- formattable::renderFormattable({
      table <- data.frame(missing_name)
      formattable::formattable(
        table,
        align = c(
          "c",
          rep(
            "l",
            NCOL(
              table
            ) - 1
          )
        )
      )
    })
    
    
    table <- data.frame(
      names,
      total_records,
      missing_records,
      records_percentage
    )
    
    customRed <- "#ff7f7f"
    
    unit.scale = function(x)
      x/100
    
    formattable::formattable(
      table,
      align = c(
        "l",
        rep(
          "r",
          NCOL(
            table
          ) - 1
        )
      ),
      list(
        records_percentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
  
  #Calculating missing data and create the table for Taxonomic Tab
  output$taxonomic_table <- formattable::renderFormattable({
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    df <- dataset_missing()
    names <- vector()
    missing_name <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    taxonomic_columns <-
      c(
        "kingdom",
        "phylum",
        "order",
        "family",
        "genus",
        "species",
        "name",
        "taxonRank",
        "scientificName",
        "taxonKey",
        "speciesKey",
        "identifiedBy",
        "dateIdentified",
        "recordedBy",
        "recordNumber",
        "typeStatus"
      )
    
    for(i in taxonomic_columns){
      if(i %in% colnames(df)){
        names <- c(names,i)
        total_records <- c(
          total_records,
          nrow(df[i])
        )
        missing_records <- c(
          missing_records,
          sum(
            is.na(
              df[i]
            )
          )
        )
        records_percentage <- c(
          records_percentage,
          round(
            (
              (
                nrow(
                  df[i]
                ) - sum(
                  is.na(
                    df[i]
                  )
                )
              ) /
                nrow(
                  df[i]
                )
            ),
            2
          ) * 100
        )
      } else {
        missing_name <- c(missing_name,i)
      }
    }
    
    output$taxonomic_missing <- formattable::renderFormattable({
      table <- data.frame(missing_name)
      formattable::formattable(
        table,
        align = c(
          "c",
          rep(
            "l",
            NCOL(
              table
            ) - 1
          )
        )
      )
    })
    
    table <- data.frame(
      names,
      total_records,
      missing_records,
      records_percentage
    )
    
    customRed <- "#ff7f7f"
    
    unit.scale = function(x)
      x/100
    
    formattable::formattable(
      table,
      align = c(
        "l",
        rep(
          "r",
          NCOL(
            table
          ) - 1
        )
      ),
      list(
        records_percentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
  
  #Missing Data Record
  
  output$temporal_missing <- renderText(missing_temporal)
  output$taxonomic <- renderText(missing_taxonomic)
  
  
  
  observeEvent(input$first_tabset, {
    updateTabsetPanel(session, "second_tabset",
                      selected = input$first_tabset
    )
  })
  
  observeEvent(input$second_tabset, {
    updateTabsetPanel(session, "first_tabset",
                      selected = input$second_tabset
    )
  })
  
}
    
## To be copied in the UI
# mod_missing_data_ui("missing_data_ui_1")
    
## To be copied in the server
# callModule(mod_missing_data_server, "missing_data_ui_1")
 
