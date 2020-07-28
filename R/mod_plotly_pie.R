#' plotly_pie UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_pie_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        mod_plot_field_selector_ui(ns("plot_field_selector_ui_1"))
      ),
      column(
        9,
        uiOutput(ns("next_ui")),
        uiOutput(ns("previous_ui"))
      )
    ),
    fluidRow(
      br(),
      uiOutput(ns("back")),
      
      plotlyOutput(ns("plot")),
      hr()
    )
    
    
  )
  
  
}

#' plotly_pie Server Function
#'
#' @noRd 
mod_plotly_pie_server <- function(input, output, session, data_reactive, data_original, column_name){
  ns <- session$ns
  
  
  plot <- reactiveValues(page_number = 1, suspended = TRUE)
  
  
  preselected <- reactiveValues(default_fields = list(x=column_name), new_fields = list(Select_X=column_name))
  
  callModule(mod_plot_field_selector_server, "plot_field_selector_ui_1", data_reactive, preselected, plot_type = "pie" )
  
  
  
  
  
  output$plot <- renderPlotly({
    if(!is.null(preselected$new_fields$Select_X)){
      
      column_x <- preselected$new_fields$Select_X
      
      chunk2 <- function(x,n) split(x, ceiling(seq_along(x)/n)) 
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), 10)
      
      if(length(a) < plot$page_number){
        plot$page_number = 1
      }
      
      temp_data <-  filter(
        data_reactive$data,
        data_reactive$data[[ preselected$new_fields$Select_X]] %in% a[[plot$page_number]])
      
      if(plot$suspended) {
        observer$resume()
        plot$suspended <- FALSE
      }
      
      future({
        dat <- as.data.frame(table("a"=temp_data[[column_x]]))
        dat
      }) %...>%
        
        plot_ly(
          type='pie',
          labels=~a,
          values= ~Freq, 
          key = ~a, 
          showlegend = FALSE,
          source = ns("tab1")
        ) %...>%
        layout(
          # title = preselected$new_fields$Select_X,
          paper_bgcolor = 'transparent',
          plot_bgcolor = "transparent",
          showlegend = FALSE,
          xaxis = list(
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
      
    }
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
    temp_data <- data_original
    for(val in data_reactive$events){
      temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
    }
    data_reactive$data <- temp_data
  })
  
  
  observer <- observeEvent(event_data("plotly_click", source = ns("tab1")), ignoreNULL = FALSE, suspended = TRUE, {
    
    
    event <- event_data("plotly_click", source = ns("tab1"))
    
    
    
    if(!is.null(event)){
      data_reactive$events[[ns("tab1")]] <- list(event$key, preselected$new_fields$Select_X)
      temp_data <- data_original
      
      for(val in data_reactive$events){
        temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
      }
      data_reactive$data <- temp_data
    }
  })
}

## To be copied in the UI
# mod_plotly_pie_ui("plotly_pie_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_pie_server, "plotly_pie_ui_1")
 
