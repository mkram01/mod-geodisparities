##############################################
# Code author: erin r stearns
# Code objective: MoD GeoDisparities web tool - Server function for dev app
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
  appdatadf <- appdata
  makeReactiveBinding('appdatadf')

  # Defining model and contextual variable reactives -------------------------------------------------------
  contextvar_ <- ''
  contextVar <- reactive({
    print('reactive: contextVar')
    contextvar_ <<- input$contextvar
    input$contextvar})
  modVar <- reactive({
    print('reactive: modVar')
    input$modvar})

  # -- updating data geography if differs from previous selection --
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters and selecting specific columns
  selected_data <- reactive({                                        #eventReactive(input$updateviz,
    print("Reactive: subset data to selected inputs")
    if (input$geolevel == "National"){
      appdatadf %>%
        filter(
          #filter to the user-selected year
          year %in% input$year
        )
      #newdf <- appdatadf[,c(contextVar(), modVar(), "county_name", "geom")]
      #names(newdf) <- c("x", "y", "county_name", "geom")
      #newdf
    } else {
      appdatadf %>%
        #filter to the user-selected state & year
        filter(
          state_name %in% input$state,
          year %in% input$year
        )
      #newdf <- appdatadf[,c(contextVar(), modVar(), "county_name", "geom")]
      #names(newdf) <- c("x", "y", "county_name", "geom")
      #newdf
    }

  })

  # Creating contextual variables base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of:
  output$contextmap <- renderLeaflet({
    print('render contextual variable map')
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
    print('render model variable map')
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
  # contextual variable map
  observeEvent(c(input$geolevel, input$state, input$year),{
    print('observeEvent: updating context df(appdata) and clearing map features')
    leafletProxy('contextmap') %>%
      clearShapes()
    appdatadf <<- selected_data()

    print(paste0("appdata df data class is: ", class(appdatadf)))

  })
  # model variable map
  observeEvent(c(input$geolevel, input$state, input$year),{
    print('observeEvent: updating mod df(appdata) and clearing map features')
    leafletProxy('modmap') %>%
      clearShapes()
    appdatadf <<- selected_data()

    print(paste0("appdata df data class is: ", class(appdatadf)))

  })

  # ---- update map bounding box ----
  # ->- conductor - parent of: map endpoint
  #               - child of:
  #left map - contextual var map
  contextcoords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - context df is: ', class(appdatadf)))
    if(input$geolevel == "National"){
      leafletProxy('contextmap') %>%
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
      print
      leafletProxy('contextmap') %>%
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }

  })
  #right map - model var map
  modcoords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - mod df is: ', class(appdatadf)))
    if(input$geolevel == "National"){
      leafletProxy('modmap') %>%
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
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


  # set map aesthetics ------------------------------------------------------------------------------------
  #---- create colorData to be able to create palette ----
  #left map - contextual var
  contextcolorData <- reactive({
    print("reactive: subsetting to contextual color var in data")
    contextdf1 <- appdatadf  #try selected_data()
    contextdf1$geom <- NULL
    contextdf1[[contextVar()]]
    # contextdf1[,contextVar()] <- round(contextdf1[,contextVar()], digits = 2)
    # contextdf1$contextquantile <- bin(contextdf1[,contextVar()], nbins = input$contextquantiles, method = "content",
    #                                   na.omit = TRUE)
    # contextdf1[,"contextquantile"]
  })

  #right map - model data var
  modcolorData <- reactive({
    print("reactive: subsetting to model color var in data")
    moddf2 <- appdatadf  #try selected_data()
    moddf2$geom <- NULL
    moddf2[[modVar()]]
    #rounding values to 2 decimal points
    # moddf2[,modVar()] <- round(moddf2[,modVar()], digits = 2)
    # #dividing var into user-specified quantiles
    # moddf2$modquantile <- bin(moddf2[,modVar()], nbins = input$modquantiles, method = "content",
    #                           na.omit = TRUE)
    # moddf2[,"modquantile"]
  })

  # contextual variable map color palette
  contextcolorpal <- reactive({
    print('reactive: create contextual var color palette')
    #colorFactor("YlGnBu", contextcolorData())
    colorBin("YlGnBu", contextcolorData(), bins = input$contextquantiles)
  })

  contextpal <- reactive({
    print('reactive: create contextual var palette for leaflet arg')
    contextcolorpal()(contextcolorData())
  })

  # contextcolorpal <- reactive({
  #   print('reactive: create contextual var color palette')
  #   print('is this working: ', (input$contextvar %in% context_continuousvars))
  #   if(input$contextvar %in% context_continuousvars){
  #     colorBin("YlGnBu", contextVar(),
  #              bins = input$contextquantiles,
  #              na.color = "#808080")
  #   } else {
  #     colorFactor("YlGnBu", contextVar(),
  #                 na.color = "#808080",
  #                 reverse = FALSE
  #                 )
  #   }
  #
  # })

  #model variable map color palette
  modcolorpal <- reactive({
    print('reactive: create right color palette')
    colorBin("BuPu", modcolorData(), bins = input$modquantiles)
  })

  modpal <- reactive({
    print('reactive: create left palette for leaflet arg')
    modcolorpal()(modcolorData())
  })

  # # ---- get reactive contextual and model data for html labels ----
  # contextData <- reactive({
  #   print("reactive: subsetting to contextual var in data")
  #   contextdf1 <- appdatadf #selected_data() #appdatadf()[[input$contextvar]] #previously used the appdatadf, may cause issues not to right now
  #   contextdf1$geom <- NULL
  #   contextdf1[,contextVar()]
  # })
  # 
  # modData <- reactive({
  #   print("reactive: subsetting to y Var in data")
  #   moddf1 <- appdatadf #selected_data() #previously used the appdatadf, may cause issues not to right now
  #   moddf1$geom <- NULL
  #   moddf1[,modVar()]
  # })
  # 
  # 
  # # ---- create html labels ----
  # #contextual var map labels
  # # child of contextData()
  # contextmap.labels <- reactive({
  #   sprintf(
  #     "%s<br/>%s<br/>",
  #     paste0(input$contextvar,": ", round(contextData(), digits = 2)), # will be problematic for categorical data
  #     paste0(input$modvar, ": ", round(modData(), digits = 2)) # will be problematic for categorical data
  #   ) %>% lapply(htmltools::HTML)
  # 
  # })
  # 
  # #model var map labels
  # # child of modData()
  # modmap.labels <- reactive({
  #   sprintf(
  #     "%s<br/>%s<br/>",
  #     paste0(input$modvar,": ", round(modData(), digits = 2)), # will be problematic for categorical data
  #     paste0(input$contextvar, ": ", round(contextData(), digits = 2)) # will be problematic for categorical data
  #   ) %>% lapply(htmltools::HTML)
  # 
  # })

  # Update maps to be choropleth of colorvars w/legends ---------------------------------------------

  #contextual var map
  observe({
    print('observe: updating contextual variable map to be choropleth of contextvar')
    print(paste0("appdatadf class: ",class(appdatadf)))
    leafletProxy('contextmap') %>%
      #counties
      addPolygons(
        data = appdatadf, #selected_data(),
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
        # label = contextmap.labels(), popup = contextmap.labels(), #~htmlEscape(input$color)
        # labelOptions = labelOptions(
        #   style = list("font-weight" = "normal", padding = "3px 8px"),
        #   textsize = "15px",
        #   direction = "auto")
      ) %>%
      #state boundaries
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

  #model var map
  observe({
    print('observe: updating left map to be chloropleth of contextvar')
    print(paste0("appdatadf class: ",class(appdatadf)))
    leafletProxy('modmap') %>%
      addPolygons(
        data = appdatadf,
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
        # label = modmap.labels(), popup = modmap.labels(), #~htmlEscape(input$color)
        # labelOptions = labelOptions(
        #   style = list("font-weight" = "normal", padding = "3px 8px"),
        #   textsize = "15px",
        #   direction = "auto")
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
  # #contextual var map
  # observe({
  #   print("observe: legend")
  #   leafletProxy("contextmap") %>%
  #     clearControls() %>%
  #     addLegend(opacity = 0.99,position = "bottomright",title = contextVar(),
  #               pal = contextcolorpal(), values = rev(contextcolorData()))
  # })
  # 
  # #model var map
  # observe({
  #   print("observe: legend")
  #   leafletProxy("modmap") %>%
  #     clearControls() %>%
  #     addLegend(opacity = 0.99,position = "bottomright",title = modVar(),
  #               pal = modcolorpal(), values = rev(modcolorData()))
  # })
  # 
  # 
  # # ---- captions -----
  # # model var map caption
  # output$modcaption <- renderUI({
  #   sprintf("<h6><b>%s</b>%s%s</h6>",
  #           as.character(input$modvar),
  #           ": ",
  #           as.character(mod_meta[mod_meta$variable %in% input$modvar, "caption_text"])
  #   ) %>% lapply(htmltools::HTML)
  # 
  #   #return(as.character(input$modvar))
  #   #return(as.character(mod_key[mod_key$variable %in% input$modvar, "caption_text"]))
  # })
  # 
  # #contextual var map caption
  # output$contextcaption <- renderUI({
  #   sprintf("<h6><b>%s</b>%s%s</h6>",
  #           as.character(input$contextvar),
  #           ": ",
  #           as.character(context_meta[context_meta$variable %in% input$contextvar, "caption_text"])
  #   ) %>% lapply(htmltools::HTML)
  #   #return(as.character(input$contextvar))
  #   #return(as.character(context_key[context_key$variable %in% input$contextvar, "caption_text"]))
  # })


  ##################################################################################
  #  create graphs -----------------------------------------------------------------
  ##################################################################################

  #create ggplot dataset
  plots_data <- reactive({
    plotsdf <- selected_data()
    #subset to only columns needed at this time
    newdf <- plotsdf[,c(contextVar(), modVar(), "county_name", "geom")]
    names(newdf) <- c("x", "y", "county_name", "geom")
    newdf
  })

  # model variable scatter plot
  output$modscatter <- renderPlotly({
    print('plotting model var scatter plot')

    mod_density <- density(plots_data()[["y"]], kernel = "gaussian", na.rm = TRUE)
    #modcol <- list(toRGB("#8856a7"))

    #create plot
    mod_uni <- plot_ly() %>%
      add_trace(x = mod_density$x, y = mod_density$y,
                type = "scatter", mode='none',
                name = input$modvar,
                fill = "tozeroy",
                fillcolor = 'rgba(136,86,167,0.5)' # hex code: "#8856a7"
      )

    # obtain plotlyjs selection
    # s <- event_data("plotly_selected")
    #
    # # if points are selected, subset the data, and highlight
    # if (length(s$x) > 0) {
    #   s_density <- density(plots_data()[["y"]], kernel = "gaussian", na.rm = TRUE)
    #   mod_uni <- add_trace(mod_uni, x = s_density$x,
    #                type = "scatter", mode = 'none',
    #                name = "Selected Scatterplot Data",
    #                fill = "tozeroy",
    #                fillcolor = 'rgba(158,188,218,0.2)'  # hex code: #9ebcda
    #                )
    # }

    mod_uni %>%
      layout(xaxis = list(title = input$modvar),
             yaxis = list(title = 'Density'))

  })

  # contextual variable scatter plot
  output$contextscatter <- renderPlotly({
    print('plotting contextual var scatter plot')

    context_density <- density(plots_data()[["x"]], kernel = "gaussian", na.rm = TRUE)
    #contextcol <- list(toRGB("#7fcdbb"))

    #create plot
    context_uni <- plot_ly() %>%
      add_trace(x = context_density$x, y = context_density$y,
                type="scatter", mode = 'none',
                name = input$contextvar,
                fill = "tozeroy",
                fillcolor = 'rgba(127,205,187,0.5' # hex code: "#7fcdbb"
                ) %>%
      layout(xaxis = list(title = input$contextvar),
             yaxis = list(title = 'Density'))

  })


  #bivariate scatter plot
  output$biscatter <- renderPlotly({
    print('plotting bivariate scatter plot')

    scatterplot <- ggplotRegression(lm(y ~ x, data = plots_data())) +
      #ggplot(plotdf(),aes(x, y)) +
      geom_point(alpha = 0.2, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
      scale_colour_distiller(palette = "YlOrRd", direction = 1) +
      theme_bw() +
      #coord_fixed()+
      theme(legend.position = "none") +
      xlab(input$contextvar) +
      ylab(input$modvar)

    #make into plotly plot
    scatterplotly <- ggplotly(scatterplot)

    #selected data
    scatterplotly <- scatterplotly %>%
      layout(dragmode = "select")

  })
  #################################################################################################
  # -------------------------- "TECHNICAL DETAILS" PAGE -------------------------------------------
  #################################################################################################



}) #close shiny server function
