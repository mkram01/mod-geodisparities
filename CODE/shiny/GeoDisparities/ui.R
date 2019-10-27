##############################################
# Code author: erin r stearns
# Code objective: MoD GeoDisparities web tool - User interface
# Date: 1.8.2019
#############################################

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
                                   <p class='parallax_description'>A tool for exploring the geography of socio-economic stressors and perinatal outcomes.</p>
                                   </section>
                                   ")
                              ),
                            
                            # ---------------- what you'll find here
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<br><br><center> <h1>What you'll find here</h1> </center><br>"),
                                     shiny::HTML("<h5>This is an interactive tool to help you visually explore the 
                                                 geographic associations between socio-economic opportunity and stress, 
                                                 and perinatal outcomes such as preterm birth.</h5>")
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
                                     shiny::HTML("<h5>This tool can help inform users about geographic patterns of 
                                                 maternal and infant risk and resilience, alongside the geographic 
                                                 patterns of community-based stressors and resources.  The tool is 
                                                 designed to offer a new way of <u><em>seeing</em></u> perinatal risk, and we hope it 
                                                 will spark conversation, communicate new insight, lead to new questions, 
                                                 and catalyze new opportunities for local collaboration and action to 
                                                 improve the health of women and infants.</h5>")
                                     ),
                              column(3)
                                     ),
                            
                            fluidRow(
                              
                              style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # ---------------- what is in the app?
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<br><br><center> <h1>What is in the app?</h1> </center><br>"),
                                     shiny::HTML("<h5>The GeoDisparities Mapper has two types of data indicators for mapping: 
                                                  <br><br>
                                                  <ol>
                                                    <li><b><u><em>Perinatal health indicators</em></u></b> – measures of the perinatal health of local 
                                                    populations are available for every county in the U.S., and are derived from a 
                                                    statistical modeling summary of vital statistics designed to produce reliable 
                                                    estimates even for sparsely populated counties. These indicators of perinatal 
                                                    health include live birth prevalence (‘risk’) of very preterm, moderately preterm, 
                                                    and overall preterm birth. In addition, there are several indicators of ‘racial 
                                                    disparity’ in each of these outcomes, because sometimes the geographic patterns of 
                                                    <u><em>high perinatal risk</u></em> are not the same as the geographic patterns of 
                                                    <u><em>large racial inequities</u></em>. </li>
                                                    <br>
                                                    <li><b><u><em>Community contextual variables</em></u></b> – measures of the 
                                                    healthcare, social, and economic context of each U.S. county are provided to 
                                                    further paint the picture of how places are different from one another. Indicators 
                                                    in this category are wide reaching, ranging from the concentration of obstetricians 
                                                    and location of Maternity Care Deserts, to the rate of poverty, violent crime, 
                                                    and housing instability.</li>
                                                  </ol></h5>")
                                     ),
                              column(3)
                                     ),
                            
                            fluidRow(
                              
                              style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # ---------------- how do you get started?
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<br><br><center> <h1>How do you get started?</h1> </center><br>"),
                                     shiny::HTML("<h5>We recommend you start by going through a brief tutorial on how to use and interpret
                                                 the perinatal and community contextual indicators, and how to use the tool. You may also 
                                                 be interested in learning about how the perinatal indicators were created by reading the 
                                                 technical documentation.
                                                 </h5>")
                                     ),
                              column(3)
                              ),
                            
                            # give 'em some space
                            fluidRow(
                              style = "height:50px;"
                            ),
                            
                            # ---------------- how do you get started: buttons; 
                            #                                 - Tutorial - 'tuts'
                            #                                 - Technical details - 'techdeets' 
                            #                                 - Contextual vars - 'contextpage'
                            #                                 - GeoDisparities Mapper - 'geomapper' 
                            fluidRow(
                              column(3),
                              column(6,
                                     
                                     
                                     tags$div(class = "wrap",
                                              #tutorial button
                                              div(class = "center", 
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("1. Tutorial",
                                                         onclick = "fakeClick('tuts')",
                                                         class="btn btn-primary btn-lg")
                                              ),
                                              #technical details button
                                              div(class = "center", 
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("2. Learn more about the perinatal outcome estimation process",
                                                         onclick = "fakeClick('perinatalpage')",
                                                         class="btn btn-primary btn-lg")
                                              ),
                                              #contextual variables button
                                              div(class = "center", 
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("3. Learn more about the community contextual variables",
                                                         onclick = "fakeClick('contextpage')",
                                                         class="btn btn-primary btn-lg")
                                              ),
                                              #geomapper button
                                              div(class = "center",
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("4. Go straight to the GeoDisparities Mapper", 
                                                         onclick="fakeClick('geomapper')", 
                                                         class="btn btn-primary btn-lg")
                                              )  
                                     ), #end tags$div
                              ), #end column 6
                              column(3)
                            ),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            
                      ), # closes "Home" tabPanel
                   
                   #################################################################################################  
                   # -------------------------- "TUTORIAL" PAGE ------------------------------------------- 
                   #################################################################################################  
                   
                   tabPanel("Tutorial", value = "tuts",
                            
                            fluidRow(
                              shiny::HTML("<br><br><center> 
                                                   <h1>GeoDisparities Mapper: A Tutorial</h1> 
                                                   </center>
                                                   <br>"),
                              style = "height:150px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # ---------------- in brief.....
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<center> <h2>In brief...</h2> </center><br>"),
                                     shiny::HTML("<h5>There are four easy steps to use the GeoDisparities Mapper app:
                                                  <br><br>
                                                  <ol>
                                                    <li>Pick a perinatal outcome (either a single indicator, or a racial disparity measure)</li>
                                                    <li>Pick a contextual indicator (either social, economic, or healthcare related)</li>
                                                    <li>(Optionally) Pick one or more states to zoom in (alternatively, view the entire U.S.)</li>
                                                    <li>Review the geographic pattern (two maps at the top) and the statistical relationship (graphs at the bottom)</li>
                                                    </ol>
                                                 </h5>")
                              ),
                              column(3)
                            ),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # ---------------- in more depth.....
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<center> <h2>In more depth...</h2> </center><br>"),
                                     shiny::HTML("<left> <h4>Place <em><b>matters</b></em></h4> </left><br>"),
                                     shiny::HTML("<h5>
                                                  <ul>
                                                    <li>Decades of research has pointed to the critical importance of the places where women 
                                                    live, work, and play as contextual regulators or both the opportunities that help make us 
                                                    <b><em>more healthy</b></em> (e.g. access to health services, neighborhoods free from 
                                                    violence, and economic opportunity), and many of the exposures that make us 
                                                    <b><em>less healthy</b></em> (e.g. violent crime, neighborhood deprivation, low social 
                                                    support, and poor housing). While many other factors such as medical risk, individual 
                                                    behaviors, and genetic predisposition are also important factors in why one woman has a 
                                                    healthy pregnancy and another women does not, it is also clear that individual behaviors 
                                                    and genetics do not explain why some <u><em>groups</u></em> of women have higher average 
                                                    risk for poor outcomes than other groups of women.</li>
                                                    <br>
                                                    <li>Because places such as counties or county-equivalents represent both an <em>environment</em>
                                                    to which women and families are exposed, and which may influence health and perinatal risk 
                                                    <u><b>and</b></u> a target for local action to improve health and health equity, examining 
                                                    the geographic variation in perinatal risk can shed new light on complex problems, and bring 
                                                    new partners to the table in discussions about improving perinatal outcomes.</li>
                                                    <br>
                                                    <li>We can see that ‘place matters’ when we look at how risk for poor outcomes varies 
                                                    dramatically between counties (hyperlink to separate pages describing the modeling procedure 
                                                    for perinatal outcomes). In the map below, it is clear that the risk for VLBW varies more than 
                                                    3-fold across U.S. counties, with clusters of higher risk counties in the South and clusters of
                                                    lower risk counties in the Northeast, Plains and Western states. Even in states with <em>“lower”</em> 
                                                    than average risk, there are still differences between counties. Why do these differences exist? In some instances it may be because of demographic profiles of counties, and in others it may be about the social, economic, and health service profiles of counties.</li>
                                                    </ul>
                                                 </h5>")
                              ),
                              column(3)
                            ),
                   ),  # Closes Technical Details tab
                   
                   #################################################################################################  
                   # -------------------------- "TECHNICAL DETAILS" PAGE ------------------------------------------- 
                   #################################################################################################  
                   
                   tabPanel("Perinatal Variables", value = "perinatalpage",
                            
                            fluidRow(
                              shiny::HTML("<br><br><center> 
                                                   <h1>GeoDisparities Mapper: Technical Details</h1> 
                                                   <h4>What's behind the modeled data.</h4>
                                                   </center>
                                                   <br>
                                                   <br>"),
                              style = "height:250px;")
                            
                   ),  # Closes Technical Details tab
                   
                   
                   #################################################################################################  
                   # -------------------------- "CONTEXTUAL VARIABLES" PAGE ------------------------------------------- 
                   #################################################################################################  
                   
                   tabPanel("Contextual Variables", value = "contextpage",
                            
                            fluidRow(
                              shiny::HTML("<br><br><center> 
                                                   <h1>GeoDisparities Mapper: Community Contextual Variables</h1> 
                                                   <h4>What's behind the contextual variables data.</h4>
                                                   </center>
                                                   <br>
                                                   <br>"),
                              style = "height:250px;")
                            
                   ),  # Closes Technical Details tab
                   
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
                                              
                                              #settings icon & hide/show button ---------------- want to figure out how to collapse other options under this if possible
                                              introBox(
                                                tags$div(
                                                  style = "height:50px;",
                                                  actionLink("settings", "Settings", 
                                                             icon = icon("sliders", class = "fa-2x"))),
                                                data.step = 6, #data steps enumerated
                                                data.intro = "Settings is where you can set options that affect the visualization of data as maps and plots."
                                              ),
                                              
                                              #select geography
                                              selectizeInput("state", "Select your geography by state",
                                                             choices = c(state_names),
                                                             selected = "Georgia",
                                                             multiple = TRUE),
                                              
                                              #update geography button
                                              actionBttn(
                                                inputId = "updategeo",
                                                label = "Update geography",
                                                color = "primary",
                                                style = "unite",
                                                #icon = icon("sliders"),
                                                size = "sm",
                                                block = TRUE
                                              ),
                                              
                                              #select year
                                              sliderInput('year', label = "Year", value = min(context_data$year), 
                                                          min = min(context_data$year), max = max(context_data$year), 
                                                          step=1, sep = ""
                                                          #,animate = animationOptions(interval = 750) #too slow with animation
                                              ),
                                              
                                              #select contextual variables (left-side) data to be mapped & plotted
                                              uiOutput('contextvar'),
                                              
                                              #select number of quantiles for left-side data
                                              sliderInput('contextquantiles', label = "Contextual Variable Quantiles", 
                                                          value = 5, min = 2, max = 10, step = 1, sep = ""
                                              ),
                                              
                                              #select model data variables (right-side) data to be mapped & plotted
                                              uiOutput('modvar'),
                                              
                                              #select number of quantiles for right-side data
                                              sliderInput('modquantiles', label = "Model Variable Quantiles", 
                                                          value = 5, min = 2, max = 10, step = 1, sep = ""
                                              ),
                                              
                                              
                                              
                                              #grab ui output
                                              uiOutput("ui")
                                              
                                            ) # end data settings for visualization
                              ),  # Closes sidebarPanel
                              
                              #Main panel -- Maps & plots controlled by settings in sidebar
                              mainPanel( width = 8,
                                    
                                         #Title left (contextual var) & right (mod var) sides
                                         fluidRow(
                                           column(6,
                                                  tags$h3("Contextual Variable")
                                                  ),
                                           column(6,
                                                  tags$h3("Modeled Outcome Variable")
                                                  )
                                         ),
                                         
                                         # PAGE BREAK
                                         tags$hr(),
                                         
                                         #Title for maps section
                                         fluidRow(
                                           column(12,
                                                  tags$h4("Chloropleth Maps"),
                                                  align="center"
                                           ),
                                           column(6,
                                                  tags$em(tags$h5("What does the geographic distribution of this contextual variable tell you?")),
                                                  align="center"
                                           ),
                                           column(6,
                                                  tags$em(tags$h5("What does the geographic distribution of this model output variable tell you?")),
                                                  align="center"
                                           )
                                           
                                         ),
                                         
                                         # maps
                                         fluidRow(
                                           column(6,
                                                  leafletOutput("contextmap")
                                           ),
                                           column(6,
                                                  leafletOutput("modmap")
                                           )
                                           
                                         ),
                                         
                                         fluidRow(
                                           
                                           style = "height:50px;"
                                           ),
                                         
                                         # PAGE BREAK
                                         tags$hr(),
                                         
                                         #Title for univariate plots section
                                         fluidRow(
                                           column(12,
                                                  tags$h4("Univariate Plots"),
                                                  align="center"
                                                  ),
                                           column(6,
                                                  tags$em(tags$h5("What does the distribution of this contextual variable tell you?")),
                                                  align="center"
                                                  ),
                                           column(6,
                                                  tags$em(tags$h5("What does the distribution of this model output variable tell you?")),
                                                  align="center"
                                                  )
                                           
                                         ),
                                         
                                         #Univariate scatter plots
                                         fluidRow(
                                           column(6,
                                                  plotOutput("contextscatter")
                                           ),
                                           column(6,
                                                  plotOutput("modscatter")
                                           )
                                         ),
                                         
                                         fluidRow(
                                           
                                           style = "height:50px;"
                                           ),
                                         
                                         # PAGE BREAK
                                         tags$hr(),
                                         
                                         #Title for bivariate plot section
                                         fluidRow(
                                           column(12,
                                                  tags$h4("Bivariate Scatter Plot"),
                                                  align="center"
                                           ),
                                           column(12,
                                                  tags$em(tags$h5(("What does the relationship between the contextual and the model outcome variables signify?"))),
                                                  align="center"
                                                  )
                                         ),
                                         
                                         #Bivariate scatter plot
                                         fluidRow(
                                           column(12,
                                                  plotOutput("biscatter",
                                                             brush = brushOpts(id="bibrush"))
                                           )
                                         ),
                                         
                                         #space
                                         fluidRow(style = "height:25px;"
                                         )
                              )  # Closes the mainPanel
                            )  # Closes the sidebarLayout
                   )  # Closes the GeoDisparities Mapper dashboard tabPanel
                   
# ------------------------------
  ) #end navbarPage
)#end UI