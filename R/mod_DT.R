#' DT UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DT_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    div(style = "display:none;",
        checkboxGroupInput(ns("checkboxgroup_spatial"), "Input checkbox 2",
                           c("Item A", "Item B", "Item C")
        )
    ),
    actionBttn(
      ns("show"),
      "Table Field Selector",
      color = "primary",
      style = "fill",
      icon = icon("tasks"), #tasks
      size = "sm"
    ),
    div(
      id = ns("summary_data_table_id"),
      DT::DTOutput(ns("summary_data_table"))
    )
  )
}

#' DT Server Function
#'
#' @noRd 
mod_DT_server <- function(input, output, session, data_reactive, pre_selected){
  ns <- session$ns
  
  # dictionary <- read.csv("data/dictionary.csv")
  group <- reactive(create_group(bddashboard::dictionary, data_reactive$data))
  
  missing <- vector()
  x <- vector()
  choices <- vector()
  a <- vector()
  previously_selected <- vector()
  first_time_pre_selected <- TRUE
  
  
  
  name_with_missing_number <- reactive({
    
    df <-data_reactive$data
    missing_name <- vector()
    names <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    
    for(i in colnames(df)){
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
      
    }
    return (setNames(as.list(records_percentage), names))
  })
  
  
  
  add_row <- function(id1, id2, col_name, selected = FALSE){
    if(col_name %in% colnames(data_reactive$data)){
      if(col_name %in% pre_selected){
        selected = TRUE
      }
      if(first_time_pre_selected && selected){
        selected = TRUE
      }else{
        selected = FALSE
      }
      fluidRow(
        column(
          6,
          style = "width: 35%;",
          progressBar(id = id1,
                      value = name_with_missing_number()[[col_name]],
                      status = "warning",
                      display_pct = TRUE,
                      striped = TRUE
          )
        ),
        column(
          6,
          div(
            id = "DT_field_selector_checkbox",
            checkboxInput(
              ns(id2),
              label = col_name,
              value = selected
            )
            
          )
        )
      )
    }
  }
  
  create_column <- function(group_name){
    column(
      2,
      style = "width: 25%; overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #828282; height: 600px;",
      fluidRow(
        column(
          2,
          h4(group_name),
        ),
        column(
          10,
          checkboxInput(
            ns(paste0("check_select_",group_name)),
            label = "Select/Deselect All",
            value = FALSE
          )
        ),
      ),
      lapply(group()[[group_name]], function(i){
        add_row(paste0("pb_",i), paste0("cb_",i), i)
      })
    )
  }
  
  
  
  
  observeEvent(input$show, {
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            div(
              style = "border-radius: 25px;border: 2px solid #828282; height: 67px;",
              column(
                3,
                div(
                  id = "core_default_btn",
                  actionBttn(
                    ns("select_default"),
                    "Select Default",
                    color = "primary",
                    style = "fill",
                    size = "sm"
                  ),
                  actionBttn(
                    ns("select_core"),
                    "Select Core",
                    color = "primary",
                    style = "fill",
                    size = "sm"
                  )
                )
              ),
              column(
                3,
                div(
                  id = "DT_select_input",
                  selectInput(
                    ns("select_input"),
                    label = "",
                    choices = c("a","b","c"),
                    selected = 'a'
                  )
                )
              ),
              column(
                3,
                div(
                  id = "select_deselect_all_checkbox",
                  checkboxInput(
                    ns("select_all_checkbox"),
                    label = "Select/Deselect All",
                    value = FALSE
                  )
                )
              ),
              column(
                3,
                div(
                  id = "DT_field_selector_icon",
                  img(src='www/DT_field_selector_icon.png', align = "right")
                )
              )
            )
          ),
          div(
            id="field_selector",
            fluidRow(
              lapply(names(group()), function(i){
                if(i!="core"){
                  create_column(i)
                }
                
              })
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "OK")
        )
      )
    )
    first_time_pre_selected <<- FALSE
  })
  
  
  
  first_time <- TRUE
  
  
  observe({
    lapply(names(group()), function(i){
      observeEvent(input[[paste0("check_select_",i)]],{
        if(input[[paste0("check_select_",i)]]){
          for(i in group()[[i]]){
            if(i %in% colnames(data_reactive$data)){
              updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
            }
          }
        }else{
          for(i in group()[[i]]){
            if(i %in% colnames(data_reactive$data)){
              updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
            }
          }
        }
      }, ignoreInit = TRUE)
    })
  })
  
  
  
  
  observeEvent(input$select_all_checkbox,{
    if(input$select_all_checkbox){
      for(i in names(group())){
        updateCheckboxInput(session, paste0("check_select_",i), value = TRUE)
      }
    }else{
      for(i in names(group())){
        updateCheckboxInput(session, paste0("check_select_",i), value = FALSE)
      }
    }
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$select_default,{
    for(i in colnames(data_reactive$data)){
      if(i %in% pre_selected){
        updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
      }else{
        updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
      }
    }
  })
  
  observeEvent(input$select_core,{
    for(i in colnames(data_reactive$data)){
      if(i %in% group()[["core"]]){
        updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
      }else{
        updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
      }
    }
  })
  
  
  
  
  # observeEvent(input$core_or_default,{
  #   if(input$core_or_default == "default"){
  #     for(i in colnames(data_reactive$data)){
  #       if(i %in% pre_selected){
  #         updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
  #       }else{
  #         updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
  #       }
  #     }
  #   }else{
  #     for(i in colnames(data_reactive$data)){
  #       if(i %in% group()[["core"]]){
  #         updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
  #       }else{
  #         updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
  #       }
  #     }
  #   }
  # },ignoreInit = TRUE)
  
  
  
  observe({
    x <- vector()
    choices <- vector()
    for(i in colnames(data_reactive$data)){
      if(!is.null(input[[paste0("cb_",i)]])){
        choices <- c(choices, i)
        if(input[[paste0("cb_",i)]]==TRUE){
          x <- c(x, i)
        }
      }
    }
    if (is.null(x))
      x <- character(0)
    
    updateCheckboxGroupInput(session, "checkboxgroup_spatial",
                             label = paste("Checkboxgroup label", length(x)),
                             choices = choices,
                             selected = x
    )
  })
  
  
  observeEvent(input[["show"]],{
    first_time <<- TRUE
    if(length(previously_selected)==0){
      for(i in colnames(data_reactive$data)){
        if(i %in% pre_selected){
          updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
        }else{
          updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
        }
      }
    }else{
      for(i in colnames(data_reactive$data)){
        if(i %in% previously_selected){
          updateCheckboxInput(session, paste0("cb_",i), value = TRUE)
        }else{
          updateCheckboxInput(session, paste0("cb_",i), value = FALSE)
        }
      }
    }
  })
  
  
  
  
  
  
  observeEvent(input$ok,{
    previously_selected <<- input$checkboxgroup_spatial
    output$summary_data_table <- DT::renderDT({
      DT::datatable(
        data_reactive$data[previously_selected],        
        filter = 'top',
        extensions = c('Buttons', "ColReorder", "Scroller"), #'Select', 'SearchPanes'
        options = list(
          scrollX = TRUE,
          dom = "Bfrtip",#'Pfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          colReorder = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE
        ),
        style = "bootstrap"
      )
    })
    removeModal()
  })
  
  
  
  filter_selected <- vector()
  output$summary_data_table <- DT::renderDT({
    data <- data_reactive$data
    
    future({
      for(i in pre_selected){
        if(i %in% colnames(data))
          filter_selected <- c(filter_selected, i)
      }
      DT::datatable(
        data[filter_selected],        
        filter = 'top',
        extensions = c('Buttons', "ColReorder", "Scroller"), #'Select', 'SearchPanes'
        options = list(
          scrollX = TRUE,
          dom = "Bfrtip",#'Pfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          colReorder = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE
        ),
        style = "bootstrap"
      )
    })
    
  })
  
}

## To be copied in the UI
# mod_DT_ui("DT_ui_1")
    
## To be copied in the server
# callModule(mod_DT_server, "DT_ui_1")
 
