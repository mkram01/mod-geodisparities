##############################################
# Code author: erin r stearns
# Code objective: MoD GeoDisparities web tool - Server function
# Date created: 8.15.2019
#############################################

# rsconnect 0.8.8

shinyServer(function(input, output, session) {

#################################################################################################
# -------------------------- "HOME"/LANDING PAGE ------------------------------------------------
#################################################################################################

  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")

  # Start Button -------------------------------------------------------------
  observeEvent(input$startBtn, {
    updateNavbarPage(session, "navBar",
                     selected = "geomapper"
    )
  })
#################################################################################################
# -------------------------- "GEODISPARITIES MAPPER"/DASHBOARD PAGE ----------------------------
#################################################################################################

  # "Take a quick tour" ----------------------------------------------------------------
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back",
                                               "skipLabel"="Exit"))
  )

  # Creating input data base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of:
  output$mapin <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>% #manually input us centroid
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")))
  })
  
  # Creating model output data base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of:
  output$mapmod <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>% #manually input us centroid
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")))
  })
  

  # Subsetting data ----------------------------------------------------------------------------------

  # -- making data into a reactive object
  df <- geodata
  makeReactiveBinding('df')

  # -- updating data geography if differs from previous selection --
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters -- aspatial for graphs
  asp_selected_data <- reactive({
    print('reactive: subset df to selected inputs')
    if (input$state == 'All'){
      geodata %>%
        filter(
          year %in% input$year
        )
    } else {
      geodata %>%
        filter(
          state_name %in% input$state,
          year %in% input$year
        )
    }

  })

  # -- updating data geography if differs from previous selection --
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters
  selected_data <- reactive({
    print('reactive: subset df to selected inputs')
    if (input$state == 'All'){
      geodata %>%
        filter(
          year %in% input$year
        )
    } else {
      geodata %>%
        filter(
          state_name %in% input$state,
          year %in% input$year
        )
    }

  })

  # ---- trigger subsetting ----
  # >- endpoint - child of: state_selected()
  #input data map
  observeEvent(c(input$state,input$year),{
    print('observeEvent: updating df and clearing map features')
    leafletProxy('mapin') %>%
      clearShapes()
    df <<- selected_data()

    print(paste0("df data class is: ", class(df)))

  })
  
  #model output map
  observeEvent(c(input$state,input$year),{
    print('observeEvent: updating df and clearing map features')
    leafletProxy('mapmod') %>%
      clearShapes()
    df <<- selected_data()
    
    print(paste0("df data class is: ", class(df)))
    
  })

  # ---- update map bounding box ---- 
  # ->- conductor - parent of: map endpoint
  #               - child of:
  #input data map
  coordsin <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - df is: ', class(df)))
    if(input$state == 'All'){
      leafletProxy('mapin') %>%
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
      print
      leafletProxy('mapin') %>%
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }

  })
  #model output map
  coordsmod <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - df is: ', class(df)))
    if(input$state == 'All'){
      leafletProxy('mapmod') %>%
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
      print
      leafletProxy('mapmod') %>%
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
    
  })
  
  # ---- adjust mapview based on state selected
  #input data map
  observeEvent(input$state, {
    coordsin()
  })
  #model output map
  observeEvent(input$state, {
    coordsmod()
  })

  #for adjusting the map view, could find centroid of subset shape and use 'setView' and those coords

  # Updating xvar, yvar & color var input options ---------------------------------------------------

  # dependent on subsetting of df above
  output$xvar <- renderUI(selectInput('xvar',label='Input Data Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1]))
  output$yvar <- renderUI(selectInput('yvar',label='Model Output Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2]))

  # Defining xvar & yvar var reactives -------------------------------------------------------
  xvar_ <- ''
  xVar <- reactive({
    print('reactive: xVar')
    if(is.null(input$xvar)) return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1])
    xvar_ <<- input$xvar
    input$xvar})
  yVar <- reactive({
    print('reactive: yVar')
    if(is.null(input$yvar)) return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2])
    input$yvar})


  # create plot dataframe -- renaming vars of interest as 'x' and 'y'
  plotdf <- reactive({
    print('plotdf')
    df1 <- `st_geometry<-`(df, NULL)
    gdf <- df1[,c(xVar(), yVar())]
    names(gdf) <- c("x","y")
    gdf
    #df1
  })
  
  # create charts -----------------------------------------------------------------
  #bivariate scatter plot 
  output$biscatter <- renderPlot({
    print('plotting bivariate scatter plot')
    
    ggplot(plotdf(),aes(x, y)) +
      geom_point(alpha = 0.5, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
      scale_colour_distiller(palette = "YlOrRd", direction = 1) +
      theme(legend.position = "none")
    
  })
  
  # xvar/input data scatter plot
  output$xscatter <- renderPlot({
    print('plotting xvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(plotdf(), input$bibrush)
    
    ggplot(plotdf(), aes(x)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  })
  
  # yvar/model output data scatter plot
  output$yscatter <- renderPlot({
    print('plotting yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(plotdf(), input$bibrush)
    
    ggplot(plotdf(), aes(y)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
  })

#################################################################################################
# -------------------------- "TECHNICAL DETAILS" PAGE -------------------------------------------
#################################################################################################



}) #close shiny server function
