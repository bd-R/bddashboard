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

#' plotly_bubble Server Function
#'
#' @noRd 
mod_plotly_bubble_server <- function(input, output, session, data_reactive, data_original, column_name=NULL, column_name_y=NULL, default_group=NULL){
  ns <- session$ns
  
  plot <- reactiveValues(page_number = 1, suspended = TRUE)
  
  preselected <- reactiveValues(default_fields = list(x=column_name, y=column_name_y), new_fields = list(Select_X=column_name, Select_Y=column_name_y))
  
  callModule(mod_plot_field_selector_server, "plot_field_selector_ui_1", data_reactive, preselected, plot_type = "bubble" )
  
  pages <- callModule(mod_plot_navigation_server, "plot_navigation_ui_1", plot, preselected, data_reactive, 30)
  
  
  
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
      data_reactive$data[[ preselected$new_fields$Select_X]] %in% pages()
    )
    
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
    
    event <-event_data("plotly_click", source = ns("tab1"))
    
    if(!is.null(event)){
      data_reactive$events[[ns("tab1")]] <- list(event$x, preselected$new_fields$Select_X)
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
# mod_plotly_bubble_ui("plotly_bubble_ui_1")
    
## To be copied in the server
# callModule(mod_plotly_bubble_server, "plotly_bubble_ui_1")
 
