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
  
  # Subsetting data ----------------------------------------------------------------------------------
  
  # -- making data into a reactive object
  df <- geodata
  makeReactiveBinding('df')
  
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
  
  
  # Creating left-side base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  output$leftmap <- renderLeaflet({
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
  
  # Creating right-side base leaflet map ---------------------------------------------------------------------------
  output$rightmap <- renderLeaflet({
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
  
  
  # ---- trigger subsetting ----
  # >- endpoint - child of: state_selected()
  #left side
  observeEvent(c(input$state,input$year),{
    print('observeEvent: updating df and clearing map features')
    leafletProxy('leftmap') %>%
      clearShapes()
    df <<- selected_data()
    
    print(paste0("df data class is: ", class(df)))
    
  })
  #right side
  observeEvent(c(input$state,input$year),{
    print('observeEvent: updating df and clearing map features')
    leafletProxy('rightmap') %>%
      clearShapes()
    df <<- selected_data()
    
    print(paste0("df data class is: ", class(df)))
    
  })
  
  # ---- update map bounding box ---- 
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  #left map
  leftcoords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - df is: ', class(df)))
    if(input$state == 'All'){
      leafletProxy('leftmap') %>% 
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
      print
      leafletProxy('leftmap') %>% 
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
    
  })
  #right map
  rightcoords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - df is: ', class(df)))
    if(input$state == 'All'){
      leafletProxy('rightmap') %>% 
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
      print
      leafletProxy('rightmap') %>% 
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
    
  })
  
  
  # ---- adjust mapview based on state selected ----
  #leftmap
  observeEvent(input$state, {
    leftcoords()
  })
  #rightmap
  observeEvent(input$state, {
    rightcoords()
  })
  
  # Update xvar & yvar var input options ---------------------------------------------------
  
  # dependent on subsetting of df above
  output$leftvar <- renderUI(selectInput('leftvar',label='Left-side Var',
                                         choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                         selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1]))
  output$rightvar <- renderUI(selectInput('rightvar',label='Right-side Var',
                                          choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                          selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2]))
  
  # Defining xvar, yvar & color var reactives -------------------------------------------------------
  xvar_ <- ''
  xVar <- reactive({
    print('reactive: xVar')
    if(is.null(input$leftvar)) return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1])
    xvar_ <<- input$leftvar
    input$leftvar})
  yVar <- reactive({
    print('reactive: yVar')
    if(is.null(input$rightvar)) return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2])
    input$rightvar})
  
  
  # create plot dataframe -- renaming vars of interest as 'x' and 'y'
  plotdf <- reactive({
    print('plotdf')
    #df1 <- `st_geometry<-`(df, NULL)
    gdf <- df[,c(xVar(), yVar(), "geometry")]
    names(gdf) <- c("x","y", "geometry")
    gdf 
    #df1
  })
  
  # create charts -----------------------------------------------------------------
  #bivariate scatter plot 
  output$biscatter <- renderPlot({
    print('plotting bivariate scatter plot')
    
    ggplotRegression(lm(x ~ y, data = plotdf())) +
      #ggplot(plotdf(),aes(x, y)) +
      geom_point(alpha = 0.5, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
      scale_colour_distiller(palette = "YlOrRd", direction = 1) +
      theme(legend.position = "none") +
      xlab(input$leftvar) +
      ylab(input$rightvar)
    
  })
  
  # xvar/input data scatter plot
  output$leftscatter <- renderPlot({
    print('plotting xvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(plotdf(), input$bibrush)
    
    ggplot(plotdf(), aes(x)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none") +
      xlab(input$leftvar)
    
  })
  
  # yvar/model output data scatter plot
  output$rightscatter <- renderPlot({
    print('plotting yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(plotdf(), input$bibrush)
    
    ggplot(plotdf(), aes(y)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none") +
      xlab(input$rightvar)
  })
  
  # set map aesthetics ------------------------------------------------------------------------------------
  #---- create colorData to be able to create palette ----
  #left map
  leftcolorData <- reactive({
    print("reactive: subsetting to colorVar in data")
    df1 <- df
    df1$geometry <- NULL
    df1[,xVar()]
    df1$leftquantile <- bin(df1[,xVar()], nbins = input$leftquantiles, method = "content")
    df1[,"leftquantile"]
  })
  
  #right map
  rightcolorData <- reactive({
    print("reactive: subsetting to colorVar in data")
    df2 <- df
    df2$geometry <- NULL
    df2[,yVar()]
    df2$rightquantile <- bin(df2[,yVar()], nbins = input$rightquantiles, method = "content")
    df2[,"rightquantile"]
  })
  
  # ---- creating palette for colorvar ----
  #left map
  leftcolorpal <- reactive({
    print('reactive: create left color palette')
    colorFactor("YlOrRd", leftcolorData())
  })
  leftpal <- reactive({
    print('reactive: create left palette for leaflet arg')
    leftcolorpal()(leftcolorData())
  })
  
  #right map
  rightcolorpal <- reactive({
    print('reactive: create right color palette')
    colorFactor("YlOrRd", rightcolorData())
  })
  rightpal <- reactive({
    print('reactive: create left palette for leaflet arg')
    rightcolorpal()(rightcolorData())
  })
  
  # ---- get reactive left(x) and right(y) data for html labels ----
  xData <- reactive({
    print("reactive: subsetting to x Var in data")
    df1 <- df
    df1$geometry <- NULL
    df1[,xVar()]
  })
  
  yData <- reactive({
    print("reactive: subsetting to y Var in data")
    df1 <- df
    df1$geometry <- NULL
    df1[,yVar()]
  })
  
  # ---- create html labels ----
  #left map
  leftmap.labels <- reactive({
    sprintf(
      "%s<br/>%s<br/>",
      paste0(input$leftvar,": ", round(xData(),digits = 2)),
      paste0(input$rightvar, ": ", round(yData(),digits = 2))
    ) %>% lapply(htmltools::HTML)
    
  })
  
  #right map
  rightmap.labels <- reactive({
    sprintf(
      "%s<br/>%s<br/>",
      paste0(input$rightvar, ": ", round(yData(),digits = 2)),
      paste0(input$leftvar,": ", round(xData(),digits = 2))
    ) %>% lapply(htmltools::HTML)
    
  })
  
  # Update map to be chloropleth of colorvar w/legend ---------------------------------------------
  
  # ---- update maps with polygons ----
  # - child of: pal()
  #left map
  observe({
    print('observe: updating left map to be chloropleth of leftVar')
    print(paste0("df class: ",class(df)))
    leafletProxy('leftmap') %>%
      addPolygons(
        data = df,
        fillColor = leftpal(),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = leftmap.labels(), popup = leftmap.labels(), #~htmlEscape(input$color)
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
  })
  
  #right map
  observe({
    print('observe: updating left map to be chloropleth of leftVar')
    print(paste0("df class: ",class(df)))
    leafletProxy('rightmap') %>%
      addPolygons(
        data = df,
        fillColor = rightpal(),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = rightmap.labels(), popup = rightmap.labels(), #~htmlEscape(input$color)
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
  })
  
  # ---- add legend ----
  #left map
  observe({
    print("observe: legend")
    leafletProxy("leftmap") %>%
      clearControls() %>%
      addLegend(opacity = 0.99,position = "bottomright",title = xVar(),
                pal = leftcolorpal(), values = rev(leftcolorData()))
  })
  
  #right map
  observe({
    print("observe: legend")
    leafletProxy("rightmap") %>%
      clearControls() %>%
      addLegend(opacity = 0.99,position = "bottomright",title = yVar(),
                pal = rightcolorpal(), values = rev(rightcolorData()))
  })
  #################################################################################################  
  # -------------------------- "TECHNICAL DETAILS" PAGE ------------------------------------------- 
  #################################################################################################  
  
  
  
}) #close shiny server function