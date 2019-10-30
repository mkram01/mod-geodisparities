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
  
  # -- making data into reactive objects
  contextdf <- context_data
  makeReactiveBinding('contextdf')
  
  moddf <- mod_data
  makeReactiveBinding('moddf')
  
  # -- updating data geography if differs from previous selection -- 
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters
  context_selected_data <- reactive({                #make into eventReactive(input$updategeo,{})
    print('reactive: subset contextdf to selected inputs')
    if (input$state == 'All'){
      context_data %>%
        filter(
          year %in% input$year
        )
    } else {
      context_data %>% 
        filter(
          state_name %in% input$state,
          year %in% input$year
        )
    }
    
  })
  
  mod_selected_data <- reactive({                     #make into eventReactive(input$updategeo,{})
    print('reactive: subset moddf to selected inputs')
    if (input$state == 'All'){
      mod_data %>%
        filter(
          year %in% input$year
        )
    } else {
      mod_data %>% 
        filter(
          state_name %in% input$state,
          year %in% input$year
        )
    }
    
  })
  
  # Creating contextual variables base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  output$contextmap <- renderLeaflet({
    print('render map')
    leaflet() %>% 
      addTiles() %>% 
      #Optional baselayers:
      #addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      #addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      #addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      #addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
      #                 options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>% #manually input us centroid
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")))
  })
  
  # Creating model variables base leaflet map ---------------------------------------------------------------------------
  output$modmap <- renderLeaflet({
    print('render map')
    leaflet() %>% 
      addTiles() %>% 
      #Optional baselayers
      #addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      #addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      #addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      #addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
      #                 options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>% #manually input us centroid
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")))
  })
  
  
  # ---- trigger subsetting ----
  # >- endpoint - child of: state_selected()
  #left side - contextual var
  observeEvent(c(input$state,input$year),{                                
    print('observeEvent: updating context df and clearing map features')
    leafletProxy('contextmap') %>%
      clearShapes()
    contextdf <<- context_selected_data()
    
    print(paste0("context df data class is: ", class(contextdf)))
    
  })
  #right side - model var
  observeEvent(c(input$state,input$year),{
    print('observeEvent: updating mod df and clearing map features')
    leafletProxy('modmap') %>%
      clearShapes()
    moddf <<- mod_selected_data()
    
    print(paste0("mod df data class is: ", class(moddf)))
    
  })
  
  # ---- update map bounding box ---- 
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  #left map - contextual var map
  contextcoords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - context df is: ', class(contextdf)))
    if(input$state == 'All'){
      leafletProxy('contextmap') %>% 
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(context_selected_data())
      print
      leafletProxy('contextmap') %>% 
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
    
  })
  #right map - model var map
  modcoords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - mod df is: ', class(moddf)))
    if(input$state == 'All'){
      leafletProxy('modmap') %>% 
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(mod_selected_data())
      print
      leafletProxy('modmap') %>% 
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
    
  })
  
  
  # ---- adjust mapview based on state selected ----
  #contextmap
  observeEvent(input$state, {
    contextcoords()
  })
  #modmap
  observeEvent(input$state, {
    modcoords()
  })
  
  # Update xvar & yvar var input options ---------------------------------------------------
  
  # dependent on subsetting of dfs above
  output$contextvar <- renderUI(selectInput('contextvar',label='Contextual Variable',
                                            choices = names(contextdf)[!names(contextdf) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                            selected =  names(contextdf)[!names(contextdf) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1]))
  output$modvar <- renderUI(selectInput('modvar',label='Perinatal Outcome Variable',
                                        choices = names(moddf)[!names(moddf) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                        selected =  names(moddf)[!names(moddf) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2]))
  
  # Defining xvar, yvar & color var reactives -------------------------------------------------------
  xvar_ <- ''
  xVar <- reactive({
    print('reactive: xVar')
    if(is.null(input$contextvar)) return(names(contextdf)[!names(contextdf) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1])
    xvar_ <<- input$contextvar
    input$contextvar})
  yVar <- reactive({
    print('reactive: yVar')
    if(is.null(input$modvar)) return(names(moddf)[!names(moddf) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2])
    input$modvar})
  
  
  # create plot dataframe -- renaming vars of interest as 'x' and 'y' ---- NOTE: because 2 separate data sets now, need to join together for this, may cause issues
  plotdf <- reactive({
    print('plotdf')
    #subsetting contextual variable df
    cdf <- contextdf[,c(xVar(), "GEOID", "geometry")]
    #subsetting model variable df
    mdf <- moddf[,c(yVar(), "GEOID", "geometry")]
    #join together
    jdf <- st_join(mdf, cdf)
    #create plot dataframe
    gdf <- jdf[,c(xVar(), yVar(), "geometry")]
    names(gdf) <- c("x","y", "geometry")
    gdf 
  })
  
  # create charts -----------------------------------------------------------------
  #bivariate scatter plot 
  output$biscatter <- renderPlot({
    print('plotting bivariate scatter plot')
    
    ggplotRegression(lm(y ~ x, data = plotdf())) +
      #ggplot(plotdf(),aes(x, y)) +
      geom_point(alpha = 0.2, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
      scale_colour_distiller(palette = "YlOrRd", direction = 1) +
      theme_bw() +
      #coord_fixed()+
      theme(legend.position = "none") +
      xlab(input$contextvar) +
      ylab(input$modvar)
    
  })
  
  # xvar/input data scatter plot
  output$contextscatter <- renderPlot({
    print('plotting contextual var scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(plotdf(), input$bibrush)
    
    ggplot(plotdf(), aes(x)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme_bw() +
      theme(legend.position = "none") +
      xlab(input$contextvar)
    
  })
  
  # yvar/model output data scatter plot
  output$modscatter <- renderPlot({
    print('plotting yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(plotdf(), input$bibrush)
    
    ggplot(plotdf(), aes(y)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme_bw() + 
      theme(legend.position = "none") +
      xlab(input$modvar)
  })
  
  # set map aesthetics ------------------------------------------------------------------------------------
  #---- create colorData to be able to create palette ----
  #left map - contextual var
  contextcolorData <- reactive({
    print("reactive: subsetting to colorVar in data")
    contextdf1 <- contextdf
    contextdf1$geometry <- NULL
    contextdf1[,xVar()] <- round(contextdf1[,xVar()], digits = 2)
    contextdf1$contextquantile <- bin(contextdf1[,xVar()], nbins = input$contextquantiles, method = "content")
    contextdf1[,"contextquantile"]
  })
  
  #right map - model data var
  modcolorData <- reactive({
    print("reactive: subsetting to colorVar in data")
    moddf2 <- moddf
    moddf2$geometry <- NULL
    #rounding values to 2 decimal points
    moddf2[,yVar()] <- round(moddf2[,yVar()], digits = 2)
    #dividing var into user-specified quantiles
    moddf2$modquantile <- bin(moddf2[,yVar()], nbins = input$modquantiles, method = "content")
    moddf2[,"modquantile"]
  })
  
  # ---- creating palette for colorvar ----
  contextkeypal <- reactive({
    print("reactive: defining color palette from metadata key")
    as.character(context_key[context_key$variable %in% input$contextvar, "palette"])
  })
  
  # contextkeyrev <- reactive({
  #   print("reactive: defining color palette direction (reverse or not) from metadata key")
  #   as.character(context_key[context_key$variable %in% input$contextvar, "reverse_palette"])
  # })
  
  #left map - contextual var
  contextcolorpal <- reactive({
    print('reactive: create contextual var color palette')
    # colorFactor(contextkeypal(), contextcolorData(), na.color = "#bdbdbd") #, reverse = contextkeyrev()
    # 
    # # key_pal <- as.character(context_key[context_key$variable %in% input$contextvar, "palette"])[[1]]
    # # reverse_pal <- as.numeric(as.character(context_key[context_key$variable %in% input$contextvar, "reverse_palette"]))[[1]]
    # # if((reverse_pal) == 1){
    # #   colorFactor((key_pal), contextcolorData(), na.color = "#bdbdbd", reverse = TRUE)
    # # } else {
    # #   colorFactor((key_pal), contextcolorData(), na.color = "#bdbdbd", reverse = FALSE)
    # # }
    colorFactor("YlGnBu", contextcolorData())
  })
  contextpal <- reactive({
    print('reactive: create contextual var palette for leaflet arg')
    contextcolorpal()(contextcolorData())
  })
  
  #right map
  modcolorpal <- reactive({
    print('reactive: create right color palette')
    colorFactor("BuPu", modcolorData())
  })
  modpal <- reactive({
    print('reactive: create left palette for leaflet arg')
    modcolorpal()(modcolorData())
  })
  
  # ---- get reactive contextual [left(x)] and model [right(y)] data for html labels ----
  xData <- reactive({
    print("reactive: subsetting to x Var in data")
    contextdf1 <- contextdf #context_selected_data() -- for eventReactive
    contextdf1$geometry <- NULL
    contextdf1[,xVar()]
  })
  
  yData <- reactive({
    print("reactive: subsetting to y Var in data")
    moddf1 <- moddf #mod_selected_data() for eventReactive
    moddf1$geometry <- NULL
    moddf1[,yVar()]
  })
  
  # ---- create html labels ----
  #left map - contextual var
  contextmap.labels <- reactive({
    sprintf(
      "%s<br/>%s<br/>",
      paste0(input$contextvar,": ", round(xData(),digits = 2)),
      paste0(input$modvar, ": ", round(yData(),digits = 2))
    ) %>% lapply(htmltools::HTML)
    
  })
  
  #right map - model var
  modmap.labels <- reactive({
    sprintf(
      "%s<br/>%s<br/>",
      paste0(input$modvar, ": ", round(yData(),digits = 2)),
      paste0(input$contextvar,": ", round(xData(),digits = 2))
    ) %>% lapply(htmltools::HTML)
    
  })
  
  # Update map to be chloropleth of colorvar w/legend ---------------------------------------------
  
  # ---- update maps with polygons ----
  # - child of: pal()
  #contextual var map
  observe({   
    print('observe: updating left map to be chloropleth of contextvar')
    print(paste0("contextdf class: ",class(contextdf)))
    #national level map -- no county boundaries, only state
    leafletProxy('contextmap') %>%
      #counties
      addPolygons(
        data = contextdf,
        fillColor = contextpal(),
        weight = 1,
        opacity = 1,
        color = contextpal(),
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = contextmap.labels(), popup = contextmap.labels(), #~htmlEscape(input$color)
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addPolygons(
        data = state_bounds,
        #fillColor = modpal(),
        fill = FALSE,
        #fillOpacity = 1,
        weight = 1,
        opacity = 1,
        color = "black",
        #dashArray = "3",
        fillOpacity = 0.7
      )
    
  })
  
  #right map
  observe({
    print('observe: updating left map to be chloropleth of contextvar')
    print(paste0("moddf class: ",class(moddf)))
    leafletProxy('modmap') %>%
      addPolygons(
        data = moddf,
        fillColor = modpal(),
        weight = 1,
        opacity = 1,
        color = modpal(),
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = modmap.labels(), popup = modmap.labels(), #~htmlEscape(input$color)
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )  %>%
      addPolygons(
        data = state_bounds,
        #fillColor = modpal(),
        fill = FALSE,
        #fillOpacity = 1,
        weight = 1,
        opacity = 1,
        color = "black",
        #dashArray = "3",
        fillOpacity = 0.7
      )
  })
  
  # ---- add legend ----
  #left map -- contextual var map
  observe({
    print("observe: legend")
    leafletProxy("contextmap") %>%
      clearControls() %>%
      addLegend(opacity = 0.99,position = "bottomright",title = xVar(),
                pal = contextcolorpal(), values = rev(contextcolorData()))
  })
  
  #right map -- model var map
  observe({
    print("observe: legend")
    leafletProxy("modmap") %>%
      clearControls() %>%
      addLegend(opacity = 0.99,position = "bottomright",title = yVar(),
                pal = modcolorpal(), values = rev(modcolorData()))
  })
  
  
  # ---- captions -----
  output$modcaption <- renderUI({
    sprintf("<h6><b>%s</b>%s%s</h6>",
            as.character(input$modvar),
            ": ",
            as.character(mod_key[mod_key$variable %in% input$modvar, "caption_text"])
    ) %>% lapply(htmltools::HTML)
    
    #return(as.character(input$modvar))
    #return(as.character(mod_key[mod_key$variable %in% input$modvar, "caption_text"]))
  })
  
  output$contextcaption <- renderUI({
    sprintf("<h6><b>%s</b>%s%s</h6>",
            as.character(input$contextvar),
            ": ",
            as.character(context_key[context_key$variable %in% input$contextvar, "caption_text"])
    ) %>% lapply(htmltools::HTML)
    #return(as.character(input$contextvar))
    #return(as.character(context_key[context_key$variable %in% input$contextvar, "caption_text"]))
  })
  
  # -------------------------- Report download ---------------------------------------------------- 
  output$downloadreport <- downloadHandler(
    filename = "GeoDisparitiesMapperReport.html",
    content = ""
  )
  
  #################################################################################################  
  # -------------------------- "TECHNICAL DETAILS" PAGE ------------------------------------------- 
  #################################################################################################  
  
  
  
}) #close shiny server function