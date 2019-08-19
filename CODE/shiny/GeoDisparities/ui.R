##############################################
# Code author: erin r stearns
# Code objective: MoD GeoDisparities web tool - User interface
# Date created: 8.15.2019
#############################################

###### TO DO ITEMS
# - figure out what the carousel panel is actually doing, on site, the image does not appear to be a carousel
# - add script for a footer


# Developed with R version 3.4.4 (64-bit)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(rgdal)
library(sf)
library(ggplot2)
library(ggvis)
library(dplyr)
library(fontawesome)
require(raster)
require(gstat)
library(stringr)
library(png)
library(shinyjs)
library(DT) #provides an R interface to the JavaScript library DataTables. R data objects (matrices or data frames) can be displayed as tables on HTML pages, and DataTables provides filtering, pagination, sorting, and many other features in the tables
library(rintrojs)

# Calling carousel script -- not sure how this is being used
source("carouselPanel.R")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

# -------------------------------------- load data -------------------------------------------------
#load spatial data
geodata <- readRDS('data/alldata.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)

#load aspatial data
#adata <- readRDS('data/acs5_2007_2017_fin.Rds')

# -------------------------------------- app inputs defined ----------------------------------------
#state choices
state_names <- as.character(unique(geodata$state_name))

######################################################################################################
# -------------------------------------- ui -------------------------------------------------------- #
######################################################################################################

shinyUI(navbarPage(title = img(src="mod.jpg", height = "40px"), id = "navBar",
                   theme = "paper.css",  # in this script, the banner image is set
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "March of Dimes GeoDisparities Mapper",
                   position = "fixed-top",
                   footer = includeHTML("./www/include_footer.html"),
                   header = tags$style(
                     ".navbar-right {
                     float: right !important;
                     }",
                       "body {padding-top: 75px;}"),

#################################################################################################
# -------------------------- "HOME"/LANDING PAGE ------------------------------------------------
#################################################################################################

                   tabPanel("Home", value = "home",

                            shinyjs::useShinyjs(),

                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(
                              HTML("
                                   <section class='banner'>
                                   <h2 class='parallax'>GeoDisparities Mapper</h2>
                                   <p class='parallax_description'>Explore associations between socio-economic stress and preterm birth.</p>
                                   </section>
                                   ")
                              ),

                            # ---------------- what you'll find here
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<br><br><center> <h1>What you'll find here</h1> </center><br>"),
                                     shiny::HTML("<h5>An interactive tool to help you explore the associations between
                                                 socio-economic opportunity and stress and preterm birth.  With information
                                                 about the impact of various social and systemic indicators on preterm and
                                                 adverse birth outcomes, programs and interventions can be tailored to
                                                 maximize impact.</h5>")
                                     ),
                              column(3)
                                     ),

                            fluidRow(

                              style = "height:50px;"),

                            # PAGE BREAK
                            tags$hr(),

                            # ---------------- how it can help you
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<br><br><center> <h1>How it can help you</h1> </center><br>"),
                                     shiny::HTML("<h5>Despite decades of clinical and population research, the problem of
                                                 large and unacceptable racial, economic, and geographic disparities in
                                                 preterm birth persist. Integrating theoretical and methodological insights
                                                 from clinical, epidemiologic and social science with program and
                                                 intervention design will accelerate impactful action to eliminate inequity
                                                 in preterm birth and other adverse perinatal outcomes..</h5>")
                                     ),
                              column(3)
                                     ),

                            fluidRow(

                              style = "height:50px;"),

                            # PAGE BREAK
                            tags$hr(),

                            # ---------------- where it came from
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<br><br><center> <h1>Where it came from</h1> </center><br>"),
                                     shiny::HTML("<h5>Our team has access currently has access to restricted-access NCHS
                                                 natality records from 1989-2016 that we used to estimate (and map) the
                                                 prevalence of very preterm birth (<32 weeks); late preterm birth (34-36 weeks);
                                                 and preterm birth (<37 weeks) for each U.S. county overall, and for
                                                 non-Hispanic White and non-Hispanic Black women separately. We  further
                                                 estimated (and mapped) the county-specific Black-White relative and absolute
                                                 disparities. To produce stable and valid estimates of both the risks and the
                                                 disparity measures we implemented spatial Bayesian disease mapping techniques,
                                                 which ‘borrow’ statistical strength for spatiotemporal dependencies. These
                                                 have been widely validated, are frequently used, and our team has used them
                                                 in previous projects for estimating perinatal and mortality parameters in US
                                                 counties.</h5>
                                                 <br>
                                                 <h5>We have assembled a unique set of county-level indicators of social and
                                                 economic context that represent potentially policy-modifiable factors that
                                                 could vary by race and geography, and that are plausibly related to known
                                                 pathways to preterm birth (e.g. they may influence social support, chronic
                                                 economic stress, and lack of quality health care). The indicators include
                                                 measures of material capital (e.g. median household income, poverty rate);
                                                 social capital (e.g. crime rate, % housing instability/turnover); human
                                                 capital (e.g. % unemployed, % changed jobs in last year, % at various levels
                                                 attained education); and local measures of economic fragility (e.g. % food
                                                 insecure, cost of living including % spending >30% on housing); and economic
                                                 safety net (e.g. % WIC, % SNAP, % of tax returns with EITC). These data come
                                                 from PolicyMap.org, the Census Bureau, and Brown University’s Diversity and
                                                 Disparities project compendium.</h5>
                                                 <br>
                                                 <h5>You will see these ecological and modeled variables in the GeoDisparities
                                                 Mapper.</h5>")
                                     ),
                              column(3)
                                     ),

                            fluidRow(

                              style = "height:50px;"),

                            # PAGE BREAK
                            tags$hr(),

                            # ---------------- how to get started
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<br><br><center> <h1>How to get started</h1> </center><br>"),
                                     shiny::HTML("<h5>To learn more about the project premise and methods or launch the
                                                 GeoDisparities Mapper, click the corresponding button below.</h5>")
                                     ),
                              column(3)
                              ),

                            # ---------------- how to get started: buttons;
                            #                                 - Technical details - 'techdeets'
                            #                                 - GeoDisparities Mapper - 'geomapper'
                            fluidRow(
                              column(3),
                              column(6,

                                     tags$div(class = "wrap",
                                              div(class = "center",
                                                  style="display: inline-block;vertical-align:top; width: 225px;",
                                                  tags$a("Learn more",
                                                         onclick = "fakeClick('techdeets')",
                                                         class="btn btn-primary btn-lg")
                                              ),
                                              div(class = "center",
                                                  style="display: inline-block; vertical-align:top; horizontal-align:middle; width: 75px;",
                                                  tags$br(), tags$h4("OR") ),
                                              div(class = "center",
                                                  style="display: inline-block;vertical-align:top; width: 225px;",
                                                  tags$a("GeoDisparities Mapper",
                                                         onclick="fakeClick('geomapper')",
                                                         class="btn btn-primary btn-lg")
                                              )
                                     )
                              ),
                              column(3)
                            ),

                            fluidRow(

                              style = "height:50px;"),

                            # PAGE BREAK
                            tags$hr(),

                            # ---------------- Understanding the drivers of disparate adverse birth outcomes (Instructional overview)
                            fluidRow(
                              shiny::HTML("<br><br><center> <h1>Understanding the drivers of disparate adverse birth outcomes.</h1> </center>
                                          <br>")
                              ),

                            fluidRow(
                              column(3),

                              column(2,
                                     div(class="panel panel-default",
                                         div(class="panel-body",  width = "600px",
                                             align = "center",
                                             div(
                                               tags$img(src = "one.svg",
                                                        width = "50px", height = "50px")
                                             ),
                                             div(
                                               h5(
                                                 "Pick a modeled outcome you would like to see visualized as a map and univariate scatter plot."
                                               )
                                             )
                                         )
                                     )
                              ),
                              column(2,
                                     div(class="panel panel-default",
                                         div(class="panel-body",  width = "600px",
                                             align = "center",
                                             div(
                                               tags$img(src = "two.svg",
                                                        width = "50px", height = "50px")
                                             ),
                                             div(
                                               h5(
                                                 "Pick an ecological predictor you would like to see visualized as a map and univariate scatter plot."
                                               )
                                             )
                                         )
                                     )
                              ),
                              column(2,
                                     div(class="panel panel-default",
                                         div(class="panel-body",  width = "600px",
                                             align = "center",
                                             div(
                                               tags$img(src = "three.svg",
                                                        width = "50px", height = "50px")),
                                             div(
                                               h5(
                                                 "Review visualizations and bivariate scatter plot comparing the two."
                                               )
                                             )
                                         )
                                     )
                              ),
                              column(3)

                            ),

                            # Embedded Video from Vimeo on how to use this tool
                            # fluidRow(
                            #     column(3),
                            #     column(6,
                            #            tags$embed(src = "https://player.vimeo.com/video/8419440",
                            #                       width = "640", height = "360")
                            #     ),
                            #     column(3)
                            # ),

                            fluidRow(

                              style = "height:50px;"),

                            # PAGE BREAK
                            tags$hr(),

                            fluidRow(shiny::HTML("<br><br><center> <h1>Ready to Get Started?</h1> </center>
                                                 <br>")
                            ),
                            fluidRow(
                              column(3),
                              column(6,
                                     tags$div(align = "center",
                                              tags$a("Start",
                                                     onclick="fakeClick('geomapper')",
                                                     class="btn btn-primary btn-lg")
                                     )
                              ),
                              column(3)
                            ),
                            fluidRow(style = "height:25px;"
                            )
                            ), # closes home tabPanel

#################################################################################################
# -------------------------- "GEODISPARITIES MAPPER"/DASHBOARD PAGE -----------------------------
#################################################################################################

                   tabPanel("GeoDisparities Mapper", value = "geomapper",

                            sidebarLayout(
                              # ----------  Mapper settings ------------------
                              sidebarPanel( width = 3,
                                            introjsUI(),

                                            tags$div(
                                              actionButton("help", "Take a Quick Tour"),
                                              style = "height:50px;"
                                            ),
                                            useShinyjs(),

                                            # ---- Data settings for visualization ----
                                            tags$div(
                                              style = "height:50px;",

                                              #settings icon & hide/show button
                                              introBox(
                                                tags$div(
                                                  style = "height:50px;",
                                                  actionLink("settings", "Settings",
                                                             icon = icon("sliders", class = "fa-2x"))),
                                                data.step = 6,
                                                data.intro = "Settings is where you can set options that affect the visualization of data as maps and plots."
                                              ),

                                              #select geography
                                              selectizeInput("state", "State",
                                                             choices = c("All",state_names),
                                                             selected = "All"),

                                              #select year
                                              sliderInput('year', label = "Year", value = 2007,
                                                          min = 2007, max = 2017,
                                                          step=1, sep = ""
                                                          #,animate = animationOptions(interval = 750) #too slow with animation
                                                          ),

                                              #select left-side data to be mapped & plotted
                                              uiOutput('xvar'),

                                              #select number of quantiles for left-side data
                                              sliderInput('xquantiles', label = "Input Data Quantiles", value = 5, min = 2, max = 10, step = 1, sep = ""
                                              ),

                                              #select right-side data to be mapped & plotted
                                              uiOutput('yvar'),

                                              #select number of quantiles for right-side data
                                              sliderInput('yquantiles', label = "Model Output Quantiles", value = 5, min = 2, max = 10, step = 1, sep = ""
                                              ),

                                              #grab ui output
                                              uiOutput("ui")

                                            ) # end data settings for visualization
                              ),  # Closes sidebarPanel

                              #Main panel -- Maps & plots controlled by settings in sidebar
                              mainPanel( width = 8,
                                         fluidRow(
                                           column(6,
                                                  leafletOutput("mapin")
                                                  ),
                                           column(6,
                                                  leafletOutput("mapmod")
                                                  )
                                           
                                         ),
                                         fluidRow(
                                           column(6,
                                                  plotOutput("xscatter")
                                                  ),
                                           column(6,
                                                  plotOutput("yscatter")
                                           )
                                         ),
                                         fluidRow(
                                           column(12,
                                                  plotOutput("biscatter",
                                                             brush = brushOpts(id="bibrush"))
                                                  )
                                         )
                              )  # Closes the mainPanel
                            )  # Closes the sidebarLayout
                   ),  # Closes the GeoDisparities Mapper dashboard tabPanel

#################################################################################################
# -------------------------- "TECHNICAL DETAILS" PAGE -------------------------------------------
#################################################################################################

                   tabPanel("Technical Details", value = "techdeets",

                            fluidRow(
                              shiny::HTML("<br><br><center>
                                                   <h1>GeoDisparities Mapper: Technical Details</h1>
                                                   <h4>What's behind the data.</h4>
                                                   </center>
                                                   <br>
                                                   <br>"),
                              style = "height:250px;")

                   )  # Closes Technical Details tab
      ) #end navbarPage
  )#end UI
