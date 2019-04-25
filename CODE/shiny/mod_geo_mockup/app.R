##############################################
# Code author: erin r stearns
# Code objective: create MoD shiny mock up
# Date: 1.8.2019
#############################################

#####################################################
# --------------- TO DO ITEMS -----------------------
# - Make auto-subsetting in response to map panning
# - Make map plot 2 vals in -- base poly color and then centroid graduated symbols
# - add Github link if open source
# ---------------------------------------------------


######################################################################################################
# -------------------------------------- set up ---------------------------------------------------- #
######################################################################################################
library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(rgdal)
library(sf)
library(ggplot2)
library(ggvis)
library(shinydashboard)
library(dplyr)
library(fontawesome)
require(raster)
require(gstat)

# -------------------------------------- load data ------------------------------------------------- 
#load spatial data
geodata <- readRDS('data/sf_acs5_2007_2017_w2010counties_v.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)

#load aspatial data
adata <- readRDS('data/acs5_2007_2017_fin.Rds')

#model data
moddata <- readRDS('data/eb_rtg.Rds')

#spatial model data
geomod <- readRDS('data/eb_spatial_v.Rds')
#transform spatial data to wgs84
geomod <- st_transform(geomod, crs = 4326)

# -------------------------------------- app inputs defined ----------------------------------------
#state choices
state_names <- as.character(unique(adata$state_name))

#state choices for modeled data
mod_state_names <- as.character(unique(moddata$state_name))

######################################################################################################
# -------------------------------------- ui -------------------------------------------------------- #
######################################################################################################
ui <- bootstrapPage(
  dashboardPage(
    skin = "green",
    dashboardHeader(title = "MoD:GeoDisparities"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("leanpub")),
        menuItem("Data explorer", tabName = "indata", icon = icon("database")),
        menuItem("Model explorer", tabName = "moddata", icon = icon("crosshairs")),
        menuItem("Social Epi of Preterm Birth", tabName = "socepi")
      )
    ),
    
    dashboardBody(
      tabItems(
        #About MoD Geodisparities ----------------------------------------------------
        tabItem(tabName = "about",
                fluidRow(
                  box(tags$img(src = "mod.jpg", height = 250, width = 225), width = 3),
                  box(includeHTML("about.html"), width = 9)
              ),
              fluidRow(
                box(
                    width = 4),
                box(
                    width = 4),
                box(
                  width = 4)
              )
              
            ), # ------------------------------------------------------- end About tab
        # Data explorer --------------------------------------------------------------
        tabItem(tabName = "indata",
                fluidRow(
                  box(
                    # -- user input options -- 
                    title = "Controls",
                    #select geography
                    # -> source - parent of:
                    selectInput('state', label = "State", choices = c("All",state_names), selected = "All"),
                    #select year
                    # -> source - parent of:
                    sliderInput('year', label = "Year", value = 2007, min = 2007, max = 2017, step=1, sep = "",
                                animate = animationOptions(interval = 750)),
                    #select xvar
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('xvar'),
                    #selectInput('xvar',label = 'x Var', choices = var_choices, selected = "blackwhite_ratio"),
                    #select yvar
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('yvar'),
                    #selectInput('yvar',label = 'y Var', choices = var_choices, selected = "edu_collegeplus"),
                    #select color var (for map)
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('cvar'),
                    #selectInput('cvar',label = 'Color Var', choices = var_choices, selected = "blackwhite_ratio"),
                    
                    #grab ui output
                    uiOutput("ui"),
                    width = 3
                    
                  ),
                  # map
                  # >- endpoint - child of:
                  box(width = 9,status = 'warning',
                      leafletOutput("map",height = 445)
                  )
                  
                ), #end fluidrow 1
                
                fluidRow(
                  # -- bivariate scatter plot -- 
                  box(plotOutput("biscatter", 
                                 brush = brushOpts(id="bibrush")), 
                      width = 6, status = 'warning'),
                  #setting row height
                  #style = 'height:40vh'
                  box(plotOutput("xscatter"),
                      width = 3, status = 'warning'),
                  box(plotOutput("yscatter"), 
                      width = 3, status = 'warning')
                ) #end fluidrow2
                
        ), # -------------------------------------------------------- end Data explorer
        
        # Model output explorer ------------------------------------------------------
        tabItem(tabName = "moddata",
                fluidRow(
                  box(
                    # -- user input options -- 
                    title = "Controls",
                    #select geography
                    # -> source - parent of:
                    selectInput('mod_state', label = "State", choices = c("All",mod_state_names), 
                                selected = "All"),
                    #select year
                    # -> source - parent of:
                    sliderInput('mod_year', label = "Year", value = 2012, min = 2012, max = 2016, 
                                step=4, sep = "",
                                animate = animationOptions(interval = 750)),
                    #select xvar
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('mod_xvar'),
                    #selectInput('xvar',label = 'x Var', choices = var_choices, selected = "blackwhite_ratio"),
                    #select yvar
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('mod_yvar'),
                    #selectInput('yvar',label = 'y Var', choices = var_choices, selected = "edu_collegeplus"),
                    #select color var (for map)
                    # -> endpoint - child of: renderUI -- not currently
                    # -> source - parent of:
                    uiOutput('mod_cvar'),
                    #selectInput('cvar',label = 'Color Var', choices = var_choices, selected = "blackwhite_ratio"),
                    
                    #grab ui output
                    uiOutput("mod_ui"),
                    width = 3
                    
                  ),
                  # map
                  # >- endpoint - child of:
                  box(width = 9,status = 'warning',
                      leafletOutput("modmap",height = 650)
                  )
                ), #end model fluidrow 1
                
                fluidRow(
                  # -- bivariate scatter plot -- 
                  box(plotOutput("mod_biscatter", 
                                 brush = brushOpts(id="mod_bibrush")), 
                      width = 6, status = 'warning'),
                  #setting row height
                  #style = 'height:40vh'
                  box(plotOutput("mod_xscatter"),
                      width = 3, status = 'warning'),
                  box(plotOutput("mod_yscatter"), 
                      width = 3, status = 'warning')
                ) #end model fluidrow 2
                
        ), # ------------------------------------------------- end Model output explorer
        
        # social epi of preterm birth ------------------------------------------------------
        tabItem(tabName = "socepi",
                fluidRow(
                  box(includeHTML("socepi_pretermbirth.html"), 
                      status = "success",
                      width = 9
                  ),
                  box(width = 3)
                ),
                fluidRow(
                  box(includeHTML("socepi_aim1.html"),
                      status = "primary", solidHeader = TRUE,
                      width = 4),
                  box(includeHTML("socepi_aim2.html"),
                      status = "warning", solidHeader = TRUE, 
                      width = 4),
                  box(includeHTML("socepi_aim3.html"),
                      status = "danger", solidHeader = TRUE, 
                      width = 4)
                  
                ),
                fluidRow(
                  box(includeHTML("socepi_ref.html"))
                )
        )
        
      ) #close tabitems
    ) #close dashboard body
  ) #close dashboard page
) #close bootstrap page


############################################
# code author: erin stearns
# script objective: spatial app server elements (separated out for testing)
# date: 5 march 2019
###########################################

######################################################################################################
# -------------------------------------- server function ------------------------------------------- #
######################################################################################################
server <- function(input, output, session) {
  
  
  ########################################################################################################
  # -------------------------------------  DATA EXPLORER -------------------------------------------------
  ########################################################################################################
  
  # Creating base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  output$map <- renderLeaflet({
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
      adata %>%
        filter(
          year %in% input$year
        )
    } else {
      adata %>% 
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
  
  #trigger subsetting
  # >- endpoint - child of: state_selected()
  observeEvent(c(input$state,input$year),{
    print('observeEvent: updating df and clearing map features')
    leafletProxy('map') %>%
      clearShapes()
    df <<- selected_data()
    
    print(paste0("df data class is: ", class(df)))
    
  })
  
  # update map bounding box
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  coords <- reactive({
    print('reactive: coords')
    print(paste0('calculating bbox - df is: ', class(df)))
    if(input$state == 'All'){
      leafletProxy('map') %>% 
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(selected_data())
      print
      leafletProxy('map') %>% 
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
    
  })
  
  #adjust mapview based on state selected
  observeEvent(input$state, {
    coords()
  })
  
  #for adjusting the map view, could find centroid of subset shape and use 'setView' and those coords
  
  # Updating xvar, yvar & color var input options ---------------------------------------------------
  
  # dependent on subsetting of df above
  output$xvar <- renderUI(selectInput('xvar',label='x Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1]))
  output$yvar <- renderUI(selectInput('yvar',label='y Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][2]))
  output$cvar <- renderUI(selectInput('color',label='Color Var',
                                      choices = names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')],
                                      selected =  names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1]))
  
  # Defining xvar, yvar & color var reactives -------------------------------------------------------
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
  colorVar <- reactive({
    print('reactive: colVar')
    if(is.null(input$color)) 
      return(names(df)[!names(df) %in% c('GEOID','state_name', 'state_code', 'year', 'source', 'NAME', 'variable', 'estimate', 'moe', 'geometry')][1])
    input$color
  })
  
  # create charts & circle size options ------------------------------------------------------------
  #bivariate scatter plot 
  output$biscatter <- renderCachedPlot({
    print('plotting bivariate scatter plot')
    
    ggplot(asp_selected_data(),aes_string(input$xvar, input$yvar, colour = input$color)) +
      geom_point(alpha = 0.5, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
      scale_colour_distiller(palette = "YlOrRd", direction = 1) +
      theme(legend.position = "none")
    
  }, cacheKeyExpr = {c(input$xvar, input$yvar, input$color)})
  
  # xvar scatter plot
  output$xscatter <- renderCachedPlot({
    print('plotting xvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(asp_selected_data(), input$bibrush)
    
    ggplot(asp_selected_data(), aes_string(input$xvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  }, cacheKeyExpr = {input$xvar})
  
  # yvar scatter plot
  output$yscatter <- renderCachedPlot({
    print('plotting yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(asp_selected_data(), input$bibrush)
    
    ggplot(asp_selected_data(), aes_string(input$yvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  }, cacheKeyExpr = {input$yvar})
  
  # map updates --------------------------------------------------------------------------------------
  #create colorData to be able to create palette
  colorData <- reactive({
    print("reactive: subsetting to colorVar in data")
    df1 <- df
    df1$geometry <- NULL
    df1[,colorVar()]
  })
  
  #creating palette for colorvar
  #creating color palette
  colorpal <- reactive({
    print('reactive: create color palette')
    colorNumeric("YlOrRd", colorData())
  })
  pal <- reactive({
    print('reactive: create palette for leaflet arg')
    colorpal()(colorData())
  })
  
  #get reactive x and y data for html labels
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
  
  
  #create html labels
  map.labels <- reactive({
    sprintf(
      "%s<br/>%s<br/>%s",
      paste0(input$xvar,": ", round(xData(),digits = 2)),
      paste0(input$yvar, ": ", round(yData(),digits = 2)),
      paste0(input$color, ": ", round(colorData(),digits = 2))
    ) %>% lapply(htmltools::HTML)
    
  })
  
  # Update map to be chloropleth of colorvar w/legend ---------------------------------------------
  
  # update map with polygons
  # - child of: pal()
  observe({
    print('observe: updating map to be chloropleth of colorvar')
    print(paste0("df class: ",class(df)))
    leafletProxy('map') %>%
      addPolygons(
        data = df,
        fillColor = pal(),
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
        label = map.labels(), popup = map.labels(), #~htmlEscape(input$color)
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
  })
  
  #add legend
  observe({
    print("observe: legend")
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(opacity = 0.99,position = "bottomright",title = colorVar(),
                pal = colorpal(), values = rev(colorData()))
  })
  
  ########################################################################################################
  # -------------------------------------  MODEL EXPLORER ------------------------------------------------
  ########################################################################################################
  
  # Creating base leaflet map ---------------------------------------------------------------------------
  # -- base map --
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  output$modmap <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>% 
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap", 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      fitBounds(-124.848974, 24.396308, -66.885444, 49.384358) %>% #manually input us bounding box
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }")))
  })
  
  # Subsetting data ----------------------------------------------------------------------------------
  
  # -- making moddata into a reactive object
  mod_df <- geomod
  makeReactiveBinding('mod_df')
  
  # -- updating data geography if differs from previous selection -- 
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters - aspatial for graphs
  mod_selected_data <- reactive({
    print('reactive: subset mod df to selected inputs')
    if (input$mod_state == 'All'){
      moddata %>%
        filter(
          Year %in% input$mod_year
        )
    } else {
      moddata %>% 
        filter(
          state_name %in% input$mod_state,
          Year %in% input$mod_year
        )
    }
    
  })
  
  # subsetting to selected data using filters -- spatial for map
  geomod_selected_data <- reactive({
    print('reactive: subset mod df to selected inputs')
    if (input$mod_state == 'All'){
      geomod %>%
        filter(
          Year %in% input$mod_year
        )
    } else {
      geomod %>% 
        filter(
          state_name %in% input$mod_state,
          Year %in% input$mod_year
        )
    }
    
  })
  
  #trigger subsetting
  # >- endpoint - child of: state_selected()
  observeEvent(c(input$mod_state,input$mod_year),{
    print('observeEvent: updating model df and clearing map features')
    leafletProxy('modmap') %>%
      clearShapes()
    mod_df <<- geomod_selected_data()
    
  })
  
  # update map bounding box
  # ->- conductor - parent of: map endpoint
  #               - child of: 
  modcoords <- reactive({
    print('reactive: mod coords')
    if(input$mod_state == 'All'){
      leafletProxy('modmap') %>%
        fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    } else {
      bbox <- st_bbox(geomod_selected_data())
      print
      leafletProxy('modmap') %>%
        fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
    }
  })
  
  #adjust mapview based on state selected
  observeEvent(input$mod_state, {
    modcoords()
  })
  
  
  # Updating xvar, yvar & color var input options ---------------------------------------------------
  
  # dependent on subsetting of df above
  output$mod_xvar <- renderUI(selectInput('mod_xvar',label='x Var',
                                          choices = names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year','geometry')],
                                          selected =  names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year','geometry')][1]))
  output$mod_yvar <- renderUI(selectInput('mod_yvar',label='y Var',
                                          choices = names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year','geometry')],
                                          selected =  names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year','geometry')][2]))
  output$mod_cvar <- renderUI(selectInput('mod_color',label='Color Var',
                                          choices = names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year','geometry')],
                                          selected =  names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year','geometry')][1]))
  
  # Defining xvar, yvar & color var reactives -------------------------------------------------------
  mod_xvar_ <- ''
  mod_xVar <- reactive({
    print('reactive: mod xVar')
    if(is.null(input$mod_xvar)) return(names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')][1])
    mod_xvar_ <<- input$mod_xvar
    input$mod_xvar})
  mod_yVar <- reactive({
    print('reactive: mod yVar')
    if(is.null(input$mod_yvar)) return(names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')][2])
    input$mod_yvar})
  mod_colorVar <- reactive({
    print('reactive: mod colVar')
    if(is.null(input$mod_color)) 
      return(names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')][1])
    input$mod_color
  })
  
  # create charts & circle size options ------------------------------------------------------------
  #bivariate scatter plot 
  output$mod_biscatter <- renderCachedPlot({
    print('plotting model bivariate scatter plot')
    
    ggplot(mod_selected_data(),aes_string(input$mod_xvar, input$mod_yvar, colour = input$mod_color)) +
      geom_point(alpha = 0.5, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red") +
      scale_colour_distiller(palette = "YlOrRd", direction = 1) +
      theme(legend.position = "none")
    
  }, cacheKeyExpr = {c(input$mod_xvar, input$mod_yvar, input$mod_color)})
  
  # xvar scatter plot
  output$mod_xscatter <- renderCachedPlot({
    print('plotting model xvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(mod_selected_data(), input$mod_bibrush)
    
    ggplot(mod_selected_data(), aes_string(input$mod_xvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  }, cacheKeyExpr = {input$mod_xvar})
  
  # yvar scatter plot
  output$mod_yscatter <- renderCachedPlot({
    print('plotting model yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(mod_selected_data(), input$mod_bibrush)
    
    ggplot(mod_selected_data(), aes_string(input$mod_yvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  }, cacheKeyExpr = {input$mod_yvar})
  
  # map updates --------------------------------------------------------------------------------------
  #create colorData to be able to create palette
  mod_colorData <- reactive({
    print("reactive: subsetting to colorVar in mod data")
    df1 <- mod_df
    df1$geometry <- NULL
    df1[,mod_colorVar()]
  })
  
  #creating palette for colorvar
  #creating color palette
  mod_colorpal <- reactive({
    print('reactive: create mod color palette')
    colorNumeric("YlOrRd", mod_colorData())
  })
  mod_pal <- reactive({
    print('reactive: create mod palette for leaflet arg')
    mod_colorpal()(mod_colorData())
  })
  
  #get reactive x and y data for html labels
  mod_xData <- reactive({
    print("reactive: subsetting to x Var in data")
    df1 <- mod_df
    df1$geometry <- NULL
    df1[,mod_xVar()]
  })
  
  mod_yData <- reactive({
    print("reactive: subsetting to y Var in data")
    df1 <- mod_df
    df1$geometry <- NULL
    df1[,mod_yVar()]
  })
  
  #create html labels
  mod_map.labels <- reactive({
    sprintf(
      "%s<br/>%s<br/>%s",
      paste0(input$mod_xvar,": ", round(mod_xData(), digits = 2)),
      paste0(input$mod_yvar, ": ", round(mod_yData(), digits = 2)),
      paste0(input$mod_color, ": ", round(mod_colorData(),digits = 2))
    ) %>% lapply(htmltools::HTML)
    
  })
  
  # Update map to be chloropleth of colorvar w/legend ---------------------------------------------
  
  # update map with polygons
  # - child of: pal()
  observe({
    print('observe: updating map to be chloropleth of colorvar')
    print(paste0("mod df class: ",class(df)))
    leafletProxy('modmap') %>%
      addPolygons(
        data = mod_df,
        fillColor = mod_pal(),
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
        label = mod_map.labels(), popup = mod_map.labels(), #~htmlEscape(input$color)
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
  })
  
  #add legend
  observe({
    print("observe: legend")
    leafletProxy("modmap") %>%
      clearControls() %>%
      addLegend(opacity = 0.99,position = "bottomright",title = mod_colorVar(),
                pal = mod_colorpal(), values = rev(mod_colorData()))
  })
  
  
} 


######################################################################################################
# -------------------------------------- shiny app call -------------------------------------------- #
######################################################################################################
shinyApp(ui = ui, server = server)
