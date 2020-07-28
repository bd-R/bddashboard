#' plotly_bubble UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plotly_bubble_ui <- function(id){
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

#' plotly_bubble Server Function
#'
#' @noRd 
mod_plotly_bubble_server <- function(input, output, session, data_reactive, data_original, column_name=NULL, column_name_y=NULL, default_group=NULL){
  ns <- session$ns
  
  plot <- reactiveValues(page_number = 1, suspended = TRUE)
  
  
  
  
  preselected <- reactiveValues(default_fields = list(x=column_name, y=column_name_y), new_fields = list(Select_X=column_name, Select_Y=column_name_y))
  
  callModule(mod_plot_field_selector_server, "plot_field_selector_ui_1", data_reactive, preselected, plot_type = "bubble" )
  
  
  
  output$plot <- renderPlotly({
    req( data_reactive$data)
    
    if(!is.null(preselected$new_fields$Select_X)){
      
      data <- data_reactive$data
      column_1 <-  preselected$new_fields$Select_X
      column_2 <-  preselected$new_fields$Select_Y
      
      chunk2 <- function(x,n) split(x, ceiling(seq_along(x)/n)) 
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), 30)
      
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
        d <- temp_data[c(column_1, column_2)]
        colnames(d) <- c("x", "y")
        d <- as.data.frame(table(d))
        d <- d %>% filter(Freq > 0) %>% droplevels()
      }) %...>%
        
        plot_ly(x = ~x, y = ~y, type = 'scatter',
                mode = 'markers', 
                span  = ~Freq,
                color = ~x,
                colors = colorRampPalette(brewer.pal(8, "Set2"))(40),
                spans = c(1, 50),
                marker = list(opacity = 0.5, sizemode = 'diameter'),
                hoverinfo = 'text',
                text = ~paste(preselected$new_fields$Select_X,": ", x, '<br>',preselected$new_fields$Select_Y,': ', y,
                              '<br> Freq: ', Freq),
                source = ns("tab1")) %...>%
        layout(paper_bgcolor = 'transparent',
               plot_bgcolor = "transparent",
               xaxis = list(
                 title = preselected$new_fields$Select_X,
                 showspikes = TRUE,
                 spikemode  = 'across',
                 spikesnap = 'cursor',
                 spikedash = "solid",
                 spikecolor = '#ffffff',
                 spikethickness = 1,
                 color = "#ffffff",
                 zeroline = FALSE,
                 showline = TRUE,
                 showticklabels = TRUE,
                 showgrid = FALSE
               ),
               yaxis = list(
                 zeroline = FALSE,
                 showline = TRUE,
                 title =  preselected$new_fields$Select_Y,
                 color = '#ffffff',
                 showticklabels = TRUE,
                 showgrid = TRUE,
                 gridcolor = toRGB("gray30")
               ),
               showlegend = FALSE
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
    
    event <-event_data("plotly_click", source = ns("tab1"))
    
    if(!is.null(event)){
      data_reactive$events[[ns("tab1")]] <- list(event$x, preselected$new_fields$Select_X)
      temp_data <- data_original
      
      for(val in data_reactive$events){
        temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
      }
      data_reactive$data <- temp_data
    }
  })
}

## To be copied in the UI
# mod_plotly_bubble_ui("plotly_bubble_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_bubble_server, "plotly_bubble_ui_1")
 
