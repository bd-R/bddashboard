#' plot_navigation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_navigation_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("last_page_ui")),
    uiOutput(ns("plus_n_ui")),
    uiOutput(ns("next_ui")),
    uiOutput(ns("page_number_ui")),
    uiOutput(ns("minus_n_ui")),
    uiOutput(ns("previous_ui")),
    uiOutput(ns("first_page_ui"))
  )
}
    
#' plot_navigation Server Function
#'
#' @noRd 
mod_plot_navigation_server <- function(input, output, session, plot, preselected, data_reactive, split_data=30){
  ns <- session$ns
  
  
  output$first_page_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)  && length(data_reactive$data)>0){
      if(plot$page_number > 1){
        div(
          style ="float:right;padding: 0px 2px 0 2px;",
          actionBttn(
            ns("first_page"),
            "<< First Page",
            style = "simple", 
            color = "primary",
            size = "sm"
          )
        )
      }
    }
  })
  
  observeEvent(input$first_page,{
      plot$page_number = 1
  })
  
  

  
  
  
  
  output$next_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)  && length(data_reactive$data)>0){
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), split_data)
      if(length(a)>1 && plot$page_number < length(a)){
        div(
          style = "float:right;padding: 0px 2px 0 2px;",
          actionBttn(
            ns("next_button"),
            "Next >",
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
  
  output$plus_n_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)  && length(data_reactive$data)>0){
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), split_data)
      if((length(a)-plot$page_number) > 10){
        div(
          style = "float:right;padding: 0px 2px 0 2px;",
          actionBttn(
            ns("plus_n_button"),
            "+10",
            style = "simple", 
            color = "primary",
            size = "sm"
          )
        )
      }
    }
  })
  
  observeEvent(input$plus_n_button,{
    plot$page_number = plot$page_number + 10
  })
  
  
  
  output$page_number_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)  && length(data_reactive$data)>0){
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), split_data)
      
      if(length(a) > 1){
          div(
            style = "float: left; padding-left: 7%;",
            verbatimTextOutput(ns("page_number"))
          )
      }
      
    }
  })
  
  output$page_number <- renderText({
    a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), split_data)
    current_page <- plot$page_number
    last_page <- length(a)
    paste0("Page",current_page,"/",last_page)
  })
  

  output$previous_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X) && length(data_reactive$data)>0){
      if(plot$page_number > 1){
        div(
          style = "float:right; padding: 0px 2px 0 2px;",
          actionBttn(
            ns("previous_button"),
            "< Previous",
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
  
  output$minus_n_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)  && length(data_reactive$data)>0){
      if(plot$page_number > 10){
        div(
          style = "float:right;padding: 0px 2px 0 2px;",
          actionBttn(
            ns("minus_n_button"),
            "-10",
            style = "simple", 
            color = "primary",
            size = "sm"
          )
        )
      }
    }
  })
  
  observeEvent(input$minus_n_button,{
    plot$page_number = plot$page_number - 10
  })
  
  output$last_page_ui <- renderUI({
    if(!is.null(preselected$new_fields$Select_X)  && length(data_reactive$data)>0){
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), split_data)
      if(plot$page_number < length(a)){
        div(
          style ="float:right;padding: 0px 2px 0 2px;",
          actionBttn(
            ns("last_page"),
            "Last Page >>",
            style = "simple", 
            color = "primary",
            size = "sm"
          )
        )
      }
    }
  })
  
  observeEvent(input$last_page,{
    if(!is.null(preselected$new_fields$Select_X)  && length(data_reactive$data)>0){
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), split_data)
      plot$page_number = length(a)
    }
    
  })
  
  
  a <- reactive({
    if(!is.null(preselected$new_fields$Select_X) && length(data_reactive$data)>0){
      a <- chunk2(unique(data_reactive$data[[preselected$new_fields$Select_X]]), split_data)
      if(length(a) < plot$page_number){
        plot$page_number = 1
      }
      return(a)
    }
  })
  

  return(reactive(a()[[plot$page_number]]))
}

chunk2 <- function(x,n) split(x, ceiling(seq_along(x)/n)) 
    
## To be copied in the UI
# mod_plot_navigation_ui("plot_navigation_ui_1")
    
## To be copied in the server
# callModule(mod_plot_navigation_server, "plot_navigation_ui_1")
 
