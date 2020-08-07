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
    column(
      6,
      selectInput(
        ns("mapTexture"),
        "Map Texture",
        choices = list(
          "OpenTopoMap" = "OpenTopoMap",
          "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
          "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
          "Stamen.Toner" = "Stamen.Toner",
          "CartoDB.Positron" = "CartoDB.Positron",
          "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
          "Stamen.Watercolor" = "Stamen.Watercolor",
          "Stamen.Terrain" = "Stamen.Terrain",
          "Esri.WorldImagery" = "Esri.WorldImagery",
          "Esri.WorldTerrain" = "Esri.WorldTerrain"
        ),
        selected = "Stamen.Watercolor"
      )
    ),
    column(
      6,
      selectInput(
        ns("mapColor"),
        "Points Color",
        choices = list(
          "Red" = 'red',
          "Green" = "green",
          "Blue" = "blue",
          "Black" = "black"
        ),
        selected = "blue"
      )
      # selectInput(
      #   ns("mapLayer"),
      #   "Points Layer",
      #   choices = c(
      #     "kingdom",
      #     "phylum",
      #     "order",
      #     "family", 
      #     "genus",
      #     "species"
      #   ),
      #   selected = "kingdom"
      #   
      # )
    ),
    leafletOutput(ns("mymap"), height = "400")
  )
}

#' leaflet Server Function
#'
#' @noRd 
mod_leaflet_server <- function(input, output, session, data_reactive, data_original){
  ns <- session$ns
  
  output$mymap <- renderLeaflet({
    
    
    
    mapLayer <- "genus"
    # 
    dat <- data_reactive$data
    # my_palette <-  brewer.pal(9, "Paired")
    # factpal <- colorFactor(my_palette, levels = unique(dat[[mapLayer]]))
    
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
    
    validate(
      need(length(dat)>0, 'Please upload/download a dataset first')
    )
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
    map_texture <- input$mapTexture
    
    # future({
    
    
    map <- leaflet(
      data = na.omit(
        dat[c(latitudeName, longitudeName)]
      )
    ) %>%
      addProviderTiles(
        map_texture
      ) %>%
      addCircleMarkers(
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
        clusterOptions = markerClusterOptions(),
        clusterId = "quakesCluster",
        radius = 1,
        weight = 10,
        opacity = 0.5,
        fill = TRUE,
        # fillOpacity = 0.2,
        color = input$mapColor,
        popup = dat$combined_label
      ) 
    
    # for(i in unique(dat[[mapLayer]])){
    #   data = dat[dat[[mapLayer]] == i, ]
    #   map <- map %>%addCircleMarkers(
    #     data = data,
    #     switch(
    #       longitudeName,
    #       "decimalLongitude" = ~decimalLongitude,
    #       "verbatimLongitude" = ~verbatimLongitude
    #     ),
    #     switch(
    #       latitudeName,
    #       "decimalLatitude" = ~decimalLatitude,
    #       "verbatimLatitude" = ~verbatimLatitude
    #     ),
    #     clusterOptions = markerClusterOptions(),
    #     clusterId = "quakesCluster",
    #     radius = 3, 
    #     weight = 4,
    #     opacity = 0.5,
    #     fill = TRUE, 
    #     fillOpacity = 0.2,
    #     color =~factpal(unique(dat[[mapLayer]])),
    #     group = i,
    #     popup = dat$combined_label
    #   )
    # }
    
    # names <- unique(dat[[mapLayer]])
    # 
    # if(length(names)>10){
    #   
    #   hidden_names <- names[6:length(names)]
    #   map <- map %>% hideGroup(hidden_names)
    # }
    
    
    
    
    # addCircles(
    #   
    #   switch(
    #     longitudeName,
    #     "decimalLongitude" = ~decimalLongitude,
    #     "verbatimLongitude" = ~verbatimLongitude
    #   ),
    #   switch(
    #     latitudeName,
    #     "decimalLatitude" = ~decimalLatitude,
    #     "verbatimLatitude" = ~verbatimLatitude
    #   ),
    #   color = map_color
    # ) %>%
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
      
    ) %>% addEasyButton(easyButton(
      states = list(
        easyButtonState(
          stateName="unfrozen-markers",
          icon="ion-toggle",
          title="Freeze Clusters",
          onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.disableClustering();
            btn.state('frozen-markers');
          }")
        ),
        easyButtonState(
          stateName="frozen-markers",
          icon="ion-toggle-filled",
          title="UnFreeze Clusters",
          onClick = JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'quakesCluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
        )
      )
    )) %>%
      leaflet.extras::addDrawToolbar(
        targetGroup='draw',
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        rectangleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions()
      ) 
    # %>%
    #   addLayersControl(
    #     overlayGroups = unique(dat[[mapLayer]]),
    #     options = layersControlOptions(
    #       collapsed=FALSE
    #     )
    #   ) 
    # })
    
    
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
      data_reactive$data <- data_original
      data_reactive$leaflet_data <- NULL
    }
  )
  
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
 
