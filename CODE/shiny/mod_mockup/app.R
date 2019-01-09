##############################################
# Code author: erin r stearns
# Code objective: create MoD shiny mock up
# Date: 1.8.2019
#############################################


######################################################################################################
# -------------------------------------- set up ---------------------------------------------------- #
######################################################################################################
pacman::p_load(rsconnect, dplyr, data.table, leaflet, sf, ggplot2, ggvis, shiny, shinydashboard,
               RColorBrewer)

#load data
south <- st_read(paste0('data/south_subset.gpkg')) %>%
  mutate(LABEL = paste(NAME, STATE_NAME, sep = ', '))


######################################################################################################
# -------------------------------------- define ui object ------------------------------------------ #
######################################################################################################

ui <-  bootstrapPage(
  #---------------->
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = HTML('MoD: Geodisparities Mockup')),
  #----------------<
  
  #---------------->
  dashboardSidebar(
    selectInput('state', label = 'State(s)',
                choices = c('All', 'Alabama', 'Arkansas', 'Delaware', 'District of Columbia',
                            'Florida', 'Georgia', 'Kentucky', 'Louisiana', 'Maryland',
                            'Mississippi', 'Missouri', 'North Carolina', 'South Carolina',
                            'Tennessee', 'Texas', 'Virginia', 'West Virginia'
                )),
    selectInput('xvar', label = 'x Var',
                choices = c('PctNoHS', 'DEN_BL', 'DEN_WH', 'DEN_HI', 'BW_RR_15', 'BW_RD_15')),
    
    selectInput('yvar', label = 'y Var',
                choices = c('PctNoHS', 'DEN_BL', 'DEN_WH', 'DEN_HI', 'BW_RR_15', 'BW_RD_15')),
    
    selectInput('color', label = 'Color Var',
                choices = c('PctNoHS', 'DEN_BL', 'DEN_WH', 'DEN_HI', 'BW_RR_15', 'BW_RD_15')),
    
    selectInput("pal", "Color palette", selected = 'BrBG',
                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),
    uiOutput("ui")
  ),
  #----------------<
  
  #---------------->
  dashboardBody(
    
    # -- plots --
    box(width = 6,status = 'warning',
        div(style = "height: 450px;",
            #scatter x vs y
            ggvisOutput("p")),
        fluidRow(
          column(width=6,
                 div(style = "height: 200px;",
                     #distribution of x var
                     ggvisOutput("p2"))),
          column(width=6,
                 div(style = "height: 200px;",
                     #distribution of y var
                     ggvisOutput("p3")))
        )
        
    ), 
    
    # -- map --
    box(width = 6,status = 'warning',
        leafletOutput("map",height = 650)
    )
    
  )
  #----------------<
  )
)
######################################################################################################
# -------------------------------------- define server function  ----------------------------------- #
######################################################################################################

server <- function(input, output, session) {
  
  #---------------->
  #leaflet map layout
  output$map <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>% 
      addProviderTiles("Esri.WorldGrayCanvas", group = "Esri.WorldGrayCanvas") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap",'DarkMatter (CartoDB)', 'Esri.WorldImagery'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F))
  })
  #----------------<
  
  #---------------->
  #reactively selecting data to be visualized
  df <- south
  makeReactiveBinding('df')
  
  observeEvent(input$state,{
    print('state')
    leafletProxy('map')%>%clearShapes()
    df <<- if (input$state == 'All'){
      south
    } else {south[south$STATE_NAME == input$state,]}
    i.active <<- NULL
    
  })
  #----------------<
  
  #---------------->
  #scatter layout
  ggvisdf <- reactive({
    print('ggvesdf1')
    df1 <- isolate(df)
    gdf <- df1[, c(input$xvar(), input$yvar())]
    names(gdf) <- c("x", "y")
    gdf
  })  
  #----------------<
  
  #---------------->
  # designating color var for map and palette
  colorData <- reactive({
    print(names(input))
    print('colData')
    df1 <- isolate(df)
    df1[,c(color(),'LABEL')]})
  colorpal <- reactive(colorNumeric(input$pal, colorData()))
  pal <- reactive({colorpal()(colorData())})
  #----------------<
  
  #---------------->
  #updating leaflet map with selected data
  observe({
    
    print('update map size/opa/color')
    leafletProxy('map')%>%
      addPolygons(
        data = df1,
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
        label = 'LABEL', popup = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) 
  })
  #----------------<
  
  #---------------->
  #updating leaflet map with appropriate legend
  observe({
    print('legend')
    leafletProxy("map")%>%
      clearControls() %>% 
      addLegend(opacity = 0.99,position = "bottomright",title = color,
                pal = colorpal(), values = rev(colorData()))
    
  })
  #----------------<
  
  #---------------->
  #defining tooltip across visualizations
  tooltip <- function(x) {
    ggvisHover <<- x
    if(is.null(x)) return(NULL)
    tt<<-paste0(c(xvar(),yvar()), ": ", format(x[1:2]), collapse = "<br/>")
    leafletProxy('map') %>%addControl(tt,layerId = 'tt',position = 'bottomleft')
    tt
  }
  #----------------<
  
  #---------------->
  #defining hover info popups across visualizations
  ggvisHover <- NULL
  makeReactiveBinding('ggvisHover')
  i.active <- NULL
  makeReactiveBinding('i.active')
  #----------------<
  
  #---------------->
  #reactive hovering
  observeEvent(ggvisHover,{
    h <- ggvisHover[1:2]
    i.active <<- ggvisdf()[,'x']==h[[1]]&ggvisdf()[,'y']==h[[2]]
  })
  
  observeEvent(input$map_marker_mouseover,{
    id <- as.numeric(input$map_marker_mouseover$id)
    if(!is.na(id)){
      i.active <<- id
    }
  })
  
  # observeEvent(i.active,{
  #   leafletProxy('map') %>%
  #     # removeMarker('hover') %>%
  #     addPolygons(lat=coords()[i.active,2],opacity = 1,
  #                      fillOpacity = 0,
  #                      radius = (input$size/5),
  #                      lng=coords()[i.active,1],
  #                      layerId = 'hover',weight = 6,
  #                      color = 'red') 
  # })
  
  mouseOver <- reactive({
    
    p <- ggvisdf()[i.active,c('x','y')]
    if(class(i.active)=='numeric'){tooltip(p)}
    p
  })
  
  #----------------<  
  
  #---------------->
  #plots
  # ggvisdf %>% 
  #   ggvis(~x,~y) %>%
  #   set_options(width = "auto", height = "auto", resizable=FALSE) %>%    
  #   # add_axis("x", title = xVar())  %>% 
  #   layer_points(size := input_slider(1, 100, value = 30,id='size',label = 'Size'),
  #                opacity := mapData,
  #                fill := pal) %>% 
  #   add_tooltip(tooltip, "hover") %>%
  #   layer_points(data =mouseOver,stroke:='red',size := 150,fillOpacity=0,strokeWidth:=5) %>%
  #   bind_shiny("p",'ui')
  
  ggvisdf %>% 
    ggvis(~x) %>%
    set_options(width = "auto", height = "auto", resizable=FALSE) %>%    
    add_axis("y", title = '')  %>% 
    layer_densities(fill := '#000054') %>% 
    layer_points(data =mouseOver,stroke:='red',size := 10) %>%
    bind_shiny("p2")
  
  ggvisdf %>% 
    ggvis(~y) %>%
    layer_densities(fill := '#000054') %>% 
    set_options(width = "auto", height = "auto", resizable=FALSE) %>%    
    add_axis("y", title = '')  %>% 
    layer_points(data =mouseOver,stroke:='red',size := 10) %>%
    bind_shiny("p3")
  
}

######################################################################################################
# -------------------------------------- call shiny app -------------------------------------------- #
######################################################################################################
shinyApp(ui, server)