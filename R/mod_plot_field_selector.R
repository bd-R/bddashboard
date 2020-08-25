#' plot_field_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_field_selector_ui <- function(id){
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
      "Show Field Selector",
      color = "primary",
      style = "fill",
      icon = icon("sliders"), #tasks
      size = "sm"
    )
  )
  
}
    
#' plot_field_selector Server Function
#'
#' @noRd 
mod_plot_field_selector_server <- function(input, output, session,  data_reactive, preselected, plot_type){
  ns <- session$ns
  
  
  group <- reactive(create_group(dashboard.experiment::dictionary, data_reactive$data))
  fields <- reactive(find_field_for_plot(data_reactive$data, plot_type, group()))
  
  
  
  missing <- vector()
  x <- vector()
  choices <- vector()
  a <- vector()
  
  temp <- list()
  
  
  fields_name <- reactive(names(fields()[[1]]))
  
  
  
  field <- reactiveValues(selected ="Default")
  
  
  
  
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
  
  
  
  
  
  
  observeEvent(input$show, {
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            div(
              style = "border-radius: 25px;border: 2px solid #828282;    margin-bottom: 1%; height: 67px;",
              column(
                4,
                radioGroupButtons(
                  inputId =  ns("columns"),
                  label = "",
                  choices = fields_name(),
                  checkIcon = list(
                    yes = icon("check-circle"),
                    no = icon("circle-o")
                  ),
                  selected = field$selected,
                  status = "info",
                  size = "sm",
                  direction = "horizontal",
                  individual = TRUE,
                  justified = TRUE
                )
              ),
              column(
                5,
                style = "width: 45%; margin-top: 1%;",
                verbatimTextOutput(ns("field_type"))
              ),
              column(
                2,
                div(
                  id="plot_field_selector_icon",
                  img(src='www/plot_field_selector_icon.png', align = "right")
                )
              )
            )
          ),
          div(
            id="field_selector",
            lapply(names(fields()[[1]]), function(i){
              conditionalPanel(
                sprintf("input['%s'] == '%s'", ns("columns"), i),
                fluidRow(
                  lapply(names(fields()), function(j){
                    create_column(j, i)
                  })
                )
              )
            })
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "Save & Exit")
        )
      )
    )
  })
  
  observeEvent(input$columns,{
    field$selected = input$columns
  })
  
  
  add_row <- function(id1, id2, col_name){
    selected <- FALSE
    if(col_name %in% colnames(data_reactive$data)){
      if(col_name %in% preselected$new_fields){
        selected = TRUE
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
            id = "plot_field_selector_radio_btn",
            prettyCheckbox(
              ns(id2),
              label = col_name,
              shape = "round", 
              outline = TRUE, 
              status = "info",
              value = selected
            )
          )
        )
      )
    }
  }
  
  create_column <- function(group_name, field_name){
    column(
      3,
      style = "width: 25%; overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #828282; height: 600px;",
      fluidRow(
        column(
          12,
          h4(group_name),
        )
      ),
      lapply(fields()[[group_name]][[field_name]], function(i){
        add_row(paste0("pb_",i,field_name), paste0("cb_",i,field_name), i)
      })
    )
  }
  
  observe({
    if (!is.null(input$columns)) {
      lapply(names(data_reactive$data), function(i) {
        observeEvent(input[[paste0("cb_", i, input$columns)]], {
          if(input$columns=="Default"){
            if (i %in% preselected$new_fields) {
              updatePrettyCheckbox(session, paste0("cb_", i, input$columns), value = TRUE)
            } else{
              updatePrettyCheckbox(session, paste0("cb_", i, input$columns), value = FALSE)
            }
          }else{
            if (input[[paste0("cb_", i, input$columns)]]) {
              for(j in names(data_reactive$data)){
                if(j==i){
                  temp[[input$columns]] <<- j
                }else{
                  updatePrettyCheckbox(session, paste0("cb_", j, input$columns), value = FALSE)
                }
              }
            }
          }
        })
      })
    }
  })
  
  observeEvent(input$ok,{
    lapply(names(temp), function(i){
      preselected$new_fields[[i]] = temp[[i]]
    })
    removeModal()
  })
  
  output$field_type <- renderText({
    if(plot_type == "bubble" || plot_type == "line"){
      if(input$columns=="Default"){
        "Field Type X: Character, Y: Numeric"
      }else if(input$columns=="Select_X"){
        "Field Type: Character"
      }
      else if(input$columns=="Select_Y"){
        "Field Type: Numeric"
      }
    }else if(plot_type == "pie" || plot_type == "bar"){
      if(input$columns=="Default"){
        "Field Type X: Character"
      }else if(input$columns=="Select_X"){
        "Field Type: Character"
      }
    }
  })
  
  
  
  
  
}

## To be copied in the UI
# mod_plot_field_selector_ui("plot_field_selector_ui_1")
    
## To be copied in the server
# callModule(mod_plot_field_selector_server, "plot_field_selector_ui_1")
 
