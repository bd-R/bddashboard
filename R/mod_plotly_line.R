#' plotly_line UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_line_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        mod_plot_field_selector_ui(ns("plot_field_selector_ui_1"))
      ),
    ),
    fluidRow(
      br(),
      uiOutput(ns("back")),
      
      plotlyOutput(ns("plot")),
    ),
    fluidRow(
      mod_plot_navigation_ui(ns("plot_navigation_ui_1"))
    ),
    hr()
    
  )
}

#' plotly_line Server Function
#'
#' @noRd 
mod_plotly_line_server <- function(input, output, session, data_reactive, data_original, column_name, column_name_y, type = "daily"){
  ns <- session$ns
  
  plot <- reactiveValues(page_number = 1, suspended = TRUE)
  
  
  preselected <- reactiveValues(default_fields = list(x=column_name, y=column_name_y), new_fields = list(Select_X=column_name, Select_Y=column_name_y))
  
  callModule(mod_plot_field_selector_server, "plot_field_selector_ui_1", data_reactive, preselected, plot_type = "bubble" )
  
  pages <- callModule(mod_plot_navigation_server, "plot_navigation_ui_1", plot, preselected, data_reactive, 10)
  
  
  
  output$plot <- renderPlotly({
    
    validate(
      need(length(data_original())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need(
        !is.null(preselected$new_fields$Select_X) || !is.null(preselected$new_fields$Select_Y),
        'Please Select A Column using Field Selector'
      )
    )
    
    validate(
      need(preselected$new_fields$Select_X %in% colnames(data_reactive$data), 
           'Default column X not found in data. Please select another column using Field Selector')
    )
    
    validate(
      need(preselected$new_fields$Select_Y %in% colnames(data_reactive$data), 
           'Default column Y not found in data. Please select another column using Field Selector')
    )
    
    
    data <- data_reactive$data
    column_1 <-  preselected$new_fields$Select_X
    column_2 <-  preselected$new_fields$Select_Y
    
    
    temp_data <-  filter(
      data_reactive$data,
      data_reactive$data[[ preselected$new_fields$Select_X]] %in% pages())
    
    if(plot$suspended) {
      observer$resume()
      plot$suspended <- FALSE
    }
    
    future({
      a <- list()
      data <- na.omit(temp_data[c(column_1, column_2)])
      for(i in unique(data[[column_1]])){
        dat <- filter(data, data[[column_1]]==i)
        dat <- data.frame(table(dat[[column_2]]), stringsAsFactors = FALSE) 
        dat <-  dat %>%
          mutate(cumsum = cumsum(Freq))
        a[[i]] <- dat
      }
      
      pl <- plot_ly(type="scatter", source = ns("tab1"), mode   = 'lines+markers')
      y_axis_column_name <- "Freq"
      if(type=="cumulative"){
        y_axis_column_name = "cumsum"
      }else{
        
      }
      for(i in names(a)){
        pl <- add_trace(pl, x=as.factor(a[[i]]$Var1), y=a[[i]][[y_axis_column_name]], mode = "lines+markers", name = i, key=i)
      }
      
      pl %>%
        layout(paper_bgcolor = 'transparent',
               plot_bgcolor = "transparent",
               xaxis = list(
                 title = column_2,
                 showspikes = TRUE,
                 spikemode  = 'across',
                 spikesnap = 'cursor',
                 spikedash = "solid",
                 spikecolor = '#ffffff',
                 spikethickness = 1,
                 showline=TRUE,
                 color = "#ffffff",
                 zeroline = TRUE,
                 showline = TRUE,
                 showticklabels = TRUE,
                 showgrid = FALSE,
                 tickformat='d'
               ),
               yaxis = list(
                 zeroline = FALSE,
                 showline = TRUE,
                 title = paste0(column_1, " ", y_axis_column_name),
                 color = '#ffffff',
                 showticklabels = TRUE,
                 showgrid = TRUE,
                 gridcolor = toRGB("gray50")
               ),
               legend = list(
                 x = 0,
                 y = 1,
                 orientation = 'h',
                 font = list(
                   color = "#ffffff"
                 )
               ),
               showlegend = TRUE,
               # hovermode  = 'x',
               spikedistance = 300,
               hoverdistance = 10
        )
    })
    
    
  })
  
  # populate back button if category is chosen
  
  output$back <- renderUI({
    if(!is.null(data_reactive$events[[ns("tab1")]])){
      actionBttn(
        ns("clear"),
        "Back/Reset",
        icon("chevron-left"),
        style = "simple", 
        color = "primary",
        size = "sm"
      )
    }
    
  })
  




  observeEvent(input$clear, {
    data_reactive$events[[ns("tab1")]] <- NULL
    temp_data <- data_original()
    for(val in data_reactive$events){
      temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
    }
    latitudeName <- "verbatimLatitude"
    if("decimalLatitude" %in% colnames(data_reactive$data))
    {
      latitudeName <- "decimalLatitude"
    }
    
    if(!is.null(data_reactive$leaflet_data)){
      temp_data <- temp_data[temp_data[[latitudeName]] %in% data_reactive$leaflet_data[[latitudeName]],]
    }
    data_reactive$data <- temp_data
  })
  
  
  observer <- observeEvent(event_data("plotly_click", source = ns("tab1")), ignoreNULL = FALSE, suspended = TRUE, {
    
    event <- event_data("plotly_click", source = ns("tab1"))
    
    if(!is.null(event)){
      data_reactive$events[[ns("tab1")]] <- list(event$key, preselected$new_fields$Select_X)
      temp_data <- data_original()
      
      for(val in data_reactive$events){
        temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
      }
      latitudeName <- "verbatimLatitude"
      if("decimalLatitude" %in% colnames(data_reactive$data))
      {
        latitudeName <- "decimalLatitude"
      }
      
      if(!is.null(data_reactive$leaflet_data)){
        temp_data <- temp_data[temp_data[[latitudeName]] %in% data_reactive$leaflet_data[[latitudeName]],]
      }
      data_reactive$data <- temp_data
    }
  })
}

## To be copied in the UI
# mod_plotly_line_ui("plotly_line_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_line_server, "plotly_line_ui_1")
 
