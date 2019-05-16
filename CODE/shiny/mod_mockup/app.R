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
counties <- readRDS('data/sf_acs5_2007_2017_w2010counties.Rds')
#transform spatial data to wgs84
counties <- st_transform(counties, crs = 4326)

#load aspatial data
adata <- readRDS('data/acs5_2007_2017_fin.Rds')

#model data
moddata <- readRDS('data/eb_rtg.Rds')

# -------------------------------------- app inputs defined ----------------------------------------
#state choices
state_names <- as.character(unique(adata$state_name))

#state choices for modeled data
mod_state_names <- as.character(unique(moddata$state_name))

######################################################################################################
# -------------------------------------- define ui object ------------------------------------------ #
######################################################################################################
ui <- bootstrapPage(
  dashboardPage(
    skin = "green",
    dashboardHeader(title = "MoD:GeoDisparities"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Data explorer", tabName = "indata", icon = icon("database")),
        menuItem("Model explorer", tabName = "moddata", icon = icon("crosshairs"))
      )
    ),
    
    dashboardBody(
      tabItems(
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
                  # -- bivariate scatter plot -- 
                  box(plotOutput("biscatter", 
                                 brush = brushOpts(id="bibrush")), 
                      width = 9, status = 'warning')
                  
                  #setting row height
                  #style = 'height:40vh'
                  
                ),
                fluidRow(
                  box(plotOutput("xscatter"),
                      width = 6, status = 'warning'),
                  box(plotOutput("yscatter"), 
                      width = 6, status = 'warning')
                )
                
        ), # -------------------------------------------------------- end Data explorer
        
        # Model output explorer ------------------------------------------------------
        tabItem(tabName = "moddata",
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
                          # -- bivariate scatter plot -- 
                          box(plotOutput("mod_biscatter", 
                                         brush = brushOpts(id="mod_bibrush")), 
                              width = 9, status = 'warning')
                          
                          #setting row height
                          #style = 'height:40vh'
                          
                        ),
                        fluidRow(
                          box(plotOutput("mod_xscatter"),
                              width = 6, status = 'warning'),
                          box(plotOutput("mod_yscatter"), 
                              width = 6, status = 'warning')
                        )
                        
                )
                
        )
        # ------------------------------------------------- end Model output explorer
      ) #close tabitems
    ) #close dashboard body
  ) #close dashboard page
) #close bootstrap page

######################################################################################################
# -------------------------------------- define server function  ----------------------------------- #
######################################################################################################
server <- function(input, output, session) {
  
  
  ########################################################################################################
  # -------------------------------------  DATA EXPLORER -------------------------------------------------
  ########################################################################################################
  
  # Subsetting data ----------------------------------------------------------------------------------
  
  # -- making data into a reactive object
  df <- adata
  makeReactiveBinding('df')
  
  # -- updating data geography if differs from previous selection -- 
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters
  selected_data <- reactive({
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
  
  
  #update dataset as necessary
  # observeEvent(c(input$state, input$year), {
  #   print('observe: if state or year changes, update df')
  #   df <<- selected_data()
  # })
  
  
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
  
  
  
  # subsetting spatial data for map ----------------------------------------------------------------
  # TO DO ITEM
  
  
  # create charts & circle size options ------------------------------------------------------------
  #bivariate scatter plot 
  output$biscatter <- renderPlot({
    print('plotting bivariate scatter plot')
    
    ggplot(selected_data(),aes_string(input$xvar, input$yvar)) +
      geom_point(alpha = 0.5, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red")
    
  })
  
  # xvar scatter plot
  output$xscatter <- renderPlot({
    print('plotting xvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(selected_data(), input$bibrush)
    
    ggplot(selected_data(), aes_string(input$xvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  })
  
  # yvar scatter plot
  output$yscatter <- renderPlot({
    print('plotting yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(selected_data(), input$bibrush)
    
    ggplot(selected_data(), aes_string(input$yvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  })
  
  ########################################################################################################
  # -------------------------------------  MODEL EXPLORER ------------------------------------------------
  ########################################################################################################
  # Subsetting data ----------------------------------------------------------------------------------
  
  # -- making moddata into a reactive object
  mod_df <- moddata
  makeReactiveBinding('mod_df')
  
  # -- updating data geography if differs from previous selection -- 
  # ->- conductor - parent of: map endpoint, observeEvent(input$state)
  #               - child of: state input
  # subsetting to selected data using filters
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
  
  
  #update dataset as necessary
  # observeEvent(c(input$state, input$year), {
  #   print('observe: if state or year changes, update df')
  #   df <<- selected_data()
  # })
  
  
  # Updating xvar, yvar & color var input options ---------------------------------------------------
  
  # dependent on subsetting of df above
  output$mod_xvar <- renderUI(selectInput('mod_xvar',label='x Var',
                                          choices = names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')],
                                          selected =  names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')][1]))
  output$mod_yvar <- renderUI(selectInput('mod_yvar',label='y Var',
                                          choices = names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')],
                                          selected =  names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')][2]))
  output$mod_cvar <- renderUI(selectInput('mod_color',label='Color Var',
                                          choices = names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')],
                                          selected =  names(mod_df)[!names(mod_df) %in% c('FIPS','state_name', 'Year')][1]))
  
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
  
  
  
  # subsetting spatial data for map ----------------------------------------------------------------
  # TO DO ITEM
  
  
  # create charts & circle size options ------------------------------------------------------------
  #bivariate scatter plot 
  output$mod_biscatter <- renderPlot({
    print('plotting model bivariate scatter plot')
    
    ggplot(mod_selected_data(),aes_string(input$mod_xvar, input$mod_yvar)) +
      geom_point(alpha = 0.5, size = 1.5) +
      geom_line(stat = "smooth", method = "lm", alpha = 0.3, colour = "red")
    
  })
  
  # xvar scatter plot
  output$mod_xscatter <- renderPlot({
    print('plotting model xvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(mod_selected_data(), input$mod_bibrush)
    
    ggplot(mod_selected_data(), aes_string(input$mod_xvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  })
  
  # yvar scatter plot
  output$mod_yscatter <- renderPlot({
    print('plotting model yvar scatter plot')
    
    #linking to brushed points from bivariate scatter
    brushed <- brushedPoints(mod_selected_data(), input$mod_bibrush)
    
    ggplot(mod_selected_data(), aes_string(input$mod_yvar)) +
      geom_density(alpha = 0.2, fill = "blue") +
      geom_density(data=brushed, fill = "red") +
      theme(legend.position = "none")
    
  })
  
  
} #close server function
######################################################################################################
# -------------------------------------- call shiny app -------------------------------------------- #
######################################################################################################
shinyApp(ui, server)
