#' plotly_bars UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_bars_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        mod_plot_field_selector_ui(ns("plot_field_selector_ui_1"))
      )
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

#' plotly_bars Server Function
#'
#' @noRd 
mod_plotly_bars_server <- function(input, output, session, data_reactive, data_original, column_name, orientation="v"){     
  ns <- session$ns
  
  plot <- reactiveValues(page_number = 1, suspended = TRUE)
  
  
  preselected <- reactiveValues(default_fields = list(x=column_name), new_fields = list(Select_X=column_name))
  
  callModule(mod_plot_field_selector_server, "plot_field_selector_ui_1", data_reactive, preselected, plot_type = "bar" )
  
  pages <- callModule(mod_plot_navigation_server, "plot_navigation_ui_1", plot, preselected, data_reactive, 10)
  
  
  
  output$plot <- renderPlotly({
    validate(
      need(length(data_original())>0, 'Please upload/download a dataset first')
    )

    validate(
      need(!is.null(preselected$new_fields$Select_X), 'Please Select A Column using Field Selector')
    )
    
    
    validate(
      need(preselected$new_fields$Select_X %in% colnames(data_reactive$data), 
           'Default column not found in data. Please select another column using Field Selector')
    )
    
    
    
    column_x <- preselected$new_fields$Select_X
    
    
    temp_data <-  filter(
      data_reactive$data,
      data_reactive$data[[ preselected$new_fields$Select_X]] %in% pages()
    )
    
    if(plot$suspended) {
      observer$resume()
      plot$suspended <- FALSE
    }
    
    
    future({
      dat <- as.data.frame(table("a"=temp_data[column_x]))
      dat
    }) %...>%
      
      
      plot_ly(
        x = if(orientation=="v"){~a}else{~Freq},
        y = if(orientation=="v"){~Freq}else{~a},
        color = ~a,
        colors = colorRampPalette(brewer.pal(8, "Set2"))(40),
        key = ~a,
        type = "bar",
        source = ns("tab1")) %...>%
      layout(
        paper_bgcolor = 'transparent',
        plot_bgcolor = "transparent",
        showlegend = FALSE,
        xaxis = list(
          # title = preselected$new_fields$Select_X,
          color = '#ffffff',
          zeroline = TRUE,
          showline = TRUE,
          showticklabels = TRUE,
          showgrid = FALSE
        ),
        yaxis = list(
          color = '#ffffff',
          showticklabels = TRUE,
          showgrid = FALSE
        )
      )
    
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
  
  output$next_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)){
      chunk2 <- function(x,n) split(x, ceiling(seq_along(x)/n)) 
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), 30)
      if(length(a)>1 && plot$page_number < length(a)){
        div(
          style = "float:right;",
          actionBttn(
            ns("next_button"),
            "Next",
            icon("chevron-right"),
            style = "simple", 
            color = "primary",
            size = "sm"
          )
          
        )
        
      }
    }
  })
  
  observeEvent(input$next_button,{
    
    plot$page_number = plot$page_number + 1
  })
  
  output$previous_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)){
      if(plot$page_number > 1){
        div(
          style = "float:right;",
          actionBttn(
            ns("previous_button"),
            "Previous",
            icon("chevron-left"),
            style = "simple", 
            color = "primary",
            size = "sm"
          )
          
        )
      }
    }
  })
  
  observeEvent(input$previous_button,{
    plot$page_number = plot$page_number - 1
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
# mod_plotly_bars_ui("plotly_bars_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_bars_server, "plotly_bars_ui_1")
 
