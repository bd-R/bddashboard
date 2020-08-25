#' leaflet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_leaflet_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        actionBttn(
          ns("show"),
          "Leaflet Settings",
          color = "primary",
          style = "fill",
          icon = icon("sliders"), #tasks
          size = "sm"
        ),
        br()
      )
    ),
    fluidRow(
      column(
        12,
        br(),
        uiOutput(ns("back")),
        leafletOutput(ns("mymap"), height = "400")
      )
    )
  )
}

#' leaflet Server Function
#'
#' @noRd 
mod_leaflet_server <- function(input, output, session, data_reactive, data_original, pre_selected="kingdom"){
  ns <- session$ns
  
  group <- reactive(create_group(dashboard.experiment::dictionary, data_reactive$data))
  temp <- list()
  layer <- reactiveValues(temp=NULL, final=pre_selected)
  map <- reactiveValues(mapTextureTemp="Stamen.Watercolor", mapTextureFinal="Stamen.Watercolor")
  mapSkin = list(
    "OpenTopoMap",
    "OpenStreetMap.Mapnik",
    "OpenStreetMap.BlackAndWhite",
    "Stamen.Toner",
    "CartoDB.Positron",
    "Esri.NatGeoWorldMap",
    "Stamen.Watercolor",
    "Stamen.Terrain",
    "Esri.WorldImagery",
    "Esri.WorldTerrain"
  )
  
  
  
  observeEvent(input$show, {
    showModal(
      modalDialog(
        fluidPage(
          fluidRow(
            fluidRow(
              div(
                style = "border-radius: 25px;border: 2px solid #828282;    margin-bottom: 1%; height: 67px;",
                column(
                  4,
                  radioGroupButtons(
                    inputId =  ns("columns"),
                    label = "",
                    choices = c("Map Texture"="map_texture", "Map Layer"="map_layer"),
                    checkIcon = list(
                      yes = icon("check-circle"),
                      no = icon("circle-o")
                    ),
                    selected = "map_texture",
                    status = "info",
                    size = "sm",
                    direction = "horizontal",
                    individual = TRUE,
                    justified = TRUE
                  )
                ),
                column(
                  4,
                  div(
                    id="leaflet_contoller",
                    img(src='www/leaflet_contoller_icon.png', align = "right")
                  )
                )
              )
            ),
          ),
          div(
            id="field_selector",
            conditionalPanel(
              sprintf("input['%s'] == '%s'", ns("columns"), "map_texture"),
              fluidRow(
                leafletOutput(ns("mymap_texture"), height = "300")
              ),
              fluidRow(
                
                  lapply(mapSkin, function(i){
                    create_button(i)
                  })
                
              )
            ),
            conditionalPanel(
              sprintf("input['%s'] == '%s'", ns("columns"), "map_layer"),
              fluidRow(
                lapply(names(group()), function(i){
                  if(i!="core"){
                    create_column(i)
                  }
                })
              )
            )
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok"), "Save & Exit")
        )
      )
    )
  })
  
  create_button <- function(btn_name){
    column(
      3,
      class = "map_texture",
      actionBttn(
        ns(btn_name),
        btn_name,
        color = "primary",
        style = "fill",
        size = "md"
      )
    )
  }
  
  observeEvent(input[["OpenTopoMap"]], {
    map$mapTextureTemp <- "OpenTopoMap"
  })
  
  observeEvent(input[["OpenStreetMap.Mapnik"]], {
    map$mapTextureTemp <- "OpenStreetMap.Mapnik"
  })
  
  observeEvent(input[["OpenStreetMap.BlackAndWhite"]], {
    map$mapTextureTemp <- "OpenStreetMap.BlackAndWhite"
  })
  
  observeEvent(input[["Stamen.Toner"]], {
    map$mapTextureTemp <- "Stamen.Toner"
  })
  
  observeEvent(input[["CartoDB.Positron"]], {
    map$mapTextureTemp <- "CartoDB.Positron"
  })
  
  observeEvent(input[["Esri.NatGeoWorldMap"]], {
    map$mapTextureTemp <- "Esri.NatGeoWorldMap"
  })
  
  observeEvent(input[["Stamen.Watercolor"]], {
    map$mapTextureTemp <- "Stamen.Watercolor"
  })
  
  observeEvent(input[["Stamen.Terrain"]], {
    map$mapTextureTemp <- "Stamen.Terrain"
  })
  
  observeEvent(input[["Esri.WorldImagery"]], {
    map$mapTextureTemp <- "Esri.WorldImagery"
  })
  
  observeEvent(input[["Esri.WorldTerrain"]], {
    map$mapTextureTemp <- "Esri.WorldTerrain"
  })
  

  
  
  
  output$mymap_texture <- renderLeaflet({
    leaflet(
    ) %>%
      addProviderTiles(
        map$mapTextureTemp
      ) 
  })
  
  create_column <- function(group_name){
    column(
      3,
      style = "width: 25%; overflow-y:scroll; max-height: 600px; border-radius: 25px; border: 2px solid #828282; height: 600px;",
      fluidRow(
        column(
          12,
          h4(group_name),
        )
      ),
      lapply(group()[[group_name]], function(i){
        add_row(paste0("cb_",i), i)
      })
    )
  }
  
  
  add_row <- function(id1, col_name, selected = FALSE){
    if(col_name %in% colnames(data_reactive$data)){
      if(col_name == pre_selected){
        selected = TRUE
      }
      fluidRow(
        column(
          12,
          div(
            id="leaflet_checkbox",
            prettyCheckbox(
              ns(id1),
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
  
  observe({
    if (!is.null(input$columns)) {
      lapply(names(data_reactive$data), function(i) {
        observeEvent(input[[paste0("cb_", i)]], {
            if(input[[paste0("cb_", i)]]){
              for(j in names(data_reactive$data)){
                if(j==i){
                  layer$temp <<- j
                }else{
                  updatePrettyCheckbox(session, paste0("cb_", j), value = FALSE)
                }
              }
              
            }
        })
      })
    }
  })
  
  observeEvent(input$ok,{
    map$mapTextureFinal <<- map$mapTextureTemp
    layer$final <<- layer$temp
    removeModal()
  })
  
  
  
  
  
  
  output$mymap <- renderLeaflet({
    
    validate(
      need(length(data_original())>0, 'Please upload/download a dataset first')
    )
    # 
    mapLayer <- layer$final
    dat <- data_reactive$data
    
    
    my_palette <-  brewer.pal(9, "Paired")
    factpal <- colorFactor(my_palette, levels = unique(dat[[mapLayer]]))
    
    
    
    # create columns with formatted links
    dat$google <- map_url(dat[[mapLayer]], label = "Lookup Google", type = "google")
    dat$crossref <- map_url(dat[[mapLayer]], label = "Lookup Crossref",
                            type = "crossref")
    dat$lens <- map_url(dat[[mapLayer]], label = "Lookup Patents", type = "lens")
    dat$gbif <- map_url(dat[[mapLayer]], label = "Lookup GBIF", type = "gbif")

    # combine links for use as a popup
    dat$combined_label <- paste0("<br>", "<strong>", dat[[mapLayer]],
                                 "</strong>", "</br>", "<br>", dat$google, "</br>", "<br>", dat$gbif,
                                 "</br>", "<br>", dat$crossref, "</br>", "<br>", dat$lens,
                                 "</br>")
    
 
    
    latitudeName <- "verbatimLatitude"
    longitudeName <- "verbatimLongitude"
    
    if("decimalLatitude" %in% colnames(dat))
    {
      latitudeName <- "decimalLatitude"
    }
    
    if("decimalLongitude" %in% colnames(dat))
    {
      longitudeName <- "decimalLongitude"
    }
    
    validate(
      need(longitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    validate(
      need(latitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    
    switch (latitudeName,
            "verbatimLatitude" = dat$verbatimLatitude <- as.numeric(dat$verbatimLatitude),
            "decimalLatitude" = dat$decimalLatitude <- as.numeric(dat$decimalLatitude),
    )
    switch (longitudeName,
            "verbatimLongitude" = dat$verbatimLongitude <- as.numeric(dat$verbatimLongitude),
            "decimalLongitude" = dat$decimalLongitude <- as.numeric(dat$decimalLongitude),
    )
    
    
    map_texture <- map$mapTextureFinal
    
    future({
    map <- leaflet(
      data = na.omit(
        dat[c(latitudeName, longitudeName)]
      )
    ) %>%
      addProviderTiles(
        map_texture
      ) 
    
    for(i in unique(dat[[mapLayer]])){
      data = dat[dat[[mapLayer]] == i, ]
      map <- map %>% addCircleMarkers(
        data = data,
        switch(
          longitudeName,
          "decimalLongitude" = ~decimalLongitude,
          "verbatimLongitude" = ~verbatimLongitude
        ),
        switch(
          latitudeName,
          "decimalLatitude" = ~decimalLatitude,
          "verbatimLatitude" = ~verbatimLatitude
        ),
        clusterOptions = markerClusterOptions(disableClusteringAtZoom=3),
        # clusterId = "quakesCluster",
          radius = 1,
          weight = 10,
          opacity = 0.5,
          fill = TRUE,
          fillOpacity = 0.2,
          # color = input$mapColor,
          color = ~factpal(i),
          group = i,
          popup = dat$combined_label
      )
    }
    
    # names <- unique(dat[[mapLayer]])
    # 
    # if(length(names)>10){
    #   
    #   hidden_names <- names[6:length(names)]
    #   map <- map %>% hideGroup(hidden_names)
    # }
    
    
    
    map %>% fitBounds(
      switch(
        longitudeName,
        "decimalLongitude" = ~min(decimalLongitude),
        "verbatimLongitude" = ~min(verbatimLongitude)
      ),
      switch(
        latitudeName,
        "decimalLatitude" = ~min(decimalLatitude),
        "verbatimLatitude" = ~min(verbatimLatitude)
      ),
      switch(
        longitudeName,
        "decimalLatitude" = ~max(decimalLongitude),
        "verbatimLatitude" = ~max(verbatimLongitude)
      ),
      switch(
        latitudeName,
        "decimalLatitude" = ~max(decimalLatitude),
        "verbatimLatitude" = ~max(verbatimLatitude)
      )
      
    ) %>% 
      leaflet.extras::addDrawToolbar(
        targetGroup='draw',
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        rectangleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions()
      ) %>% 
      addLayersControl(
        overlayGroups = unique(dat[[mapLayer]]),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    })
    
  })
  
  
  observeEvent(
    input$mymap_draw_new_feature,
    {
      dat <- data_reactive$data
      
      latitudeName <- "verbatimLatitude"
      longitudeName <- "verbatimLongitude"
      
      if("decimalLatitude" %in% colnames(dat))
      {
        latitudeName <- "decimalLatitude"
      }
      
      if("decimalLongitude" %in% colnames(dat))
      {
        longitudeName <- "decimalLongitude"
      }
      
      switch (latitudeName,
              "verbatimLatitude" = dat$verbatimLatitude <- as.numeric(dat$verbatimLatitude),
              "decimalLatitude" = dat$decimalLatitude <- as.numeric(dat$decimalLatitude),
      )
      switch (longitudeName,
              "verbatimLongitude" = dat$verbatimLongitude <- as.numeric(dat$verbatimLongitude),
              "decimalLongitude" = dat$decimalLongitude <- as.numeric(dat$decimalLongitude),
      )
      
      
      data <- na.omit(
        dat[c(
          latitudeName,
          longitudeName
        )]
      )
      cities_coordinates <- SpatialPointsDataFrame(
        data[,c(
          longitudeName,
          latitudeName
        )],
        data
      )
      
      #get the coordinates of the polygon
      polygon_coordinates <- 
        input$mymap_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon <- 
        Polygon(
          do.call(
            rbind,
            lapply(
              polygon_coordinates,
              function(x){c(x[[1]][1],x[[2]][1])}
            )
          )
        )
      
      #use over from the sp package to identify selected cities
      selected_cities <- 
        cities_coordinates %over% 
        SpatialPolygons(
          list(
            Polygons(
              list(
                drawn_polygon
              ),
              "drawn_polygon"
            )
          )
        )
      
      #print the name of the cities
      geo <- as.data.frame(
        dat[which(
          !is.na(
            selected_cities
          )
        ),
        colnames(
          dat
        )]
      )
      
      data_reactive$data <- geo
      data_reactive$leaflet_data <- geo
    })
  
  observeEvent(
    input$mymap_draw_deleted_features,
    {
      data_reactive$data <- data_original()
      data_reactive$leaflet_data <- NULL
    }
  )
  
  output$back <- renderUI({
    if(!is.null(data_reactive$leaflet_data)){
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
    data_reactive$leaflet_data <- NULL
    temp_data <- data_original()
    latitudeName <- "verbatimLatitude"

    if("decimalLatitude" %in% colnames(data_reactive$data))
    {
      latitudeName <- "decimalLatitude"
    }
    for(val in data_reactive$events){
      temp_data <- temp_data[temp_data[[val[[2]]]] == val[[1]],]
    }
    if(!is.null(data_reactive$leaflet_data)){
      temp_data <- temp_data[temp_data[[latitudeName]] %in% data_reactive$leaflet_data[[latitudeName]],]
    }
    data_reactive$data <- temp_data
  })
  
  
  
}

map_url <- function(query, label = "NULL", type = "NULL") {
  href <- "<a href="
  close_href <- ">"  #included for flexibility in labelling
  close_a <- "</a>"
  if (type == "google") {
    query <- stringr::str_replace_all(query, " ", "+")
    google_base <- "https://www.google.co.uk/#q="
    url <- paste0(google_base, query)
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  if (type == "crossref") {
    query <- stringr::str_replace_all(query, " ", "+%2B")
    crossref_base <- "http://search.crossref.org/?q=%2B"
    url <- paste0(crossref_base, query)
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  if (type == "gbif") {
    query <- stringr::str_replace_all(query, " ", "+")
    gbif_base <- "http://www.gbif.org/species/search?q="
    url <- paste0(gbif_base, query)
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  if (type == "lens") {
    # note restriction to main jurisdictions and no stemming to reduce
    # duplication and false positives
    query <- stringr::str_replace_all(query, " ", "+")
    lens_base <- "https://www.lens.org/lens/search?q="
    url <- paste0(lens_base, "%22", query, "%22", "&jo=true&j=EP&j=JP&j=US&j=WO&st=false&n=50")
    out <- paste0(href, shQuote(url), close_href, label, close_a)
  }
  out
}

## To be copied in the UI
# mod_leaflet_ui("leaflet_ui_1")
    
## To be copied in the server
# callModule(mod_leaflet_server, "leaflet_ui_1")
 
