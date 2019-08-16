##############################################
# Code author: erin r stearns
# Code objective: create MoD shiny mock up - ui script
# Date: 1.8.2019
#############################################

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
        menuItem("Project details", tabName = "details", icon = icon("asterisk")),
        menuItem("Data explorer", tabName = "indata", icon = icon("database")),
        menuItem("Model explorer", tabName = "moddata", icon = icon("crosshairs"))
      )
    ),
    
    dashboardBody(
      tabItems(
        #About MoD Geodisparities ----------------------------------------------------
        tabItem(tabName = "about",
                fluidRow(
                  box(tags$img(src = "mod.jpg", height = 250, width = 225), width = 3),
                  
                  #about markdown
                  box(includeMarkdown("mod_geo_mockup/about.md")
                      )
                    )
                  ), # ------------------------------------------------------- end About tab
        
        #Project Details -------------------------------------------------------------
        tabItem(tabName = "details",
                #soc epi of preterm birth
                fluidRow(
                  box(includeMarkdown("mod_geo_mockup/socepi_pretermbirth.md"),
                      width = 12)
                ),
                
                #detailing aims
                fluidRow(
                  # aim 1
                  tabBox(
                    title = includeMarkdown("mod_geo_mockup/socepi_aim1_title.md"),
                    id = "aim1_tabset", #height = "250px",
                    tabPanel("Motivation", includeMarkdown("mod_geo_mockup/socepi_aim1_motivation.md")),
                    tabPanel("Methodology", includeMarkdown("mod_geo_mockup/socepi_aim1_methodology.md")),
                    width = 4
                  ),
                  #aim 2
                  tabBox(
                    title = includeMarkdown("mod_geo_mockup/socepi_aim2_title.md"),
                    id = "aim2_tabset", #height = "250px",
                    tabPanel("Motivation", includeMarkdown("mod_geo_mockup/socepi_aim2_motivation.md")),
                    tabPanel("Methodology", includeMarkdown("mod_geo_mockup/socepi_aim2_methodology.md")),
                    width = 4
                  ),
                  
                  #aim 3
                  box(includeMarkdown("mod_geo_mockup/socepi_aim3.md"),
                      width = 4)
                ),
                #citations
                fluidRow(
                  box(includeMarkdown("mod_geo_mockup/socepi_ref.md"),
                      width = 12
                  )
                )
                ), # ------------------------------------------------------- end Project details tab
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
                    sliderInput('year', label = "Year", value = 2007, min = 2007, max = 2017, step=1, sep = ""
                                #,animate = animationOptions(interval = 750) #too slow with animation
                                ),
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
                    
                    #select number of quantiles
                    sliderInput('quantiles', label = "Quantiles", value = 5, min = 2, max = 10, step = 1, sep = ""
                                ),
                    
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
                                step=4, sep = ""
                                #, animate = animationOptions(interval = 750) #currently does not make sense
                                ),
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
                    
                    #select number of quantiles
                    sliderInput('mod_quantiles', label = "Quantiles", value = 5, min = 2, max = 10, step = 1, sep = ""
                    ),
                    
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
                
        ) # ------------------------------------------------- end Model output explorer
        
      ) #close tabitems
    ) #close dashboard body
  ) #close dashboard page
) #close bootstrap page


