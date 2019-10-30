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
                                     shiny::HTML("<br><br><center> <h1>What will you find here?</h1> </center><br>"),
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
                                     shiny::HTML("<br><br><center> <h1>How can it help you?</h1> </center><br>"),
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
                              column(2),
                              
                              column(2,  #tutorial
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "600px",
                                             align = "center",
                                             div(
                                               tags$h5("Learn how to use and interpret the GeoDisparities Mapper with the Tutorial!")
                                             )
                                         )
                                     )
                              ),
                              column(2,
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "600px",
                                             align = "center",
                                             div(
                                               tags$h5("Explore geodisparities and go straight to the GeoDisparities Mapper!")
                                             )
                                         )
                                     )
                              ),
                              
                              column(2,
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "600px",
                                             align = "center",
                                             div(
                                               tags$h5("Learn more about the community contextual variables!")
                                             )
                                         )
                                     )
                              ),
                              
                              column(2,
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "600px",
                                             align = "center",
                                             div(
                                               tags$h5("Learn more about the perinatal outcome estimation process!")
                                             )
                                         )
                                     )
                              ),
                              
                              column(2)
                            ),
                            
                            
                            fluidRow(
                              column(2),
                              
                              column(2,
                                     tags$div(class = "wrap",
                                              #tutorial button
                                              div(class = "center", 
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("Go!",
                                                         onclick = "fakeClick('tuts')",
                                                         class="btn btn-primary btn-lg")
                                              )
                                     )
                              ),
                              column(2,
                                     tags$div(class="wrap",
                                              #technical details button
                                              div(class = "center", 
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("Go!",
                                                         onclick = "fakeClick('geomapper')",
                                                         class="btn btn-primary btn-lg")
                                              )
                                     )
                              ),
                              column(2,
                                     tags$div(class="wrap",
                                              #contextual variables button
                                              div(class = "center", 
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("Go!",
                                                         onclick = "fakeClick('contextpage')",
                                                         class="btn btn-primary btn-lg")
                                              )
                                     )
                              ),
                              column(2,
                                     tags$div(class="wrap",
                                              #geomapper button
                                              div(class = "center",
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("Go!", 
                                                         onclick="fakeClick('perinatalpage')", 
                                                         class="btn btn-primary btn-lg")
                                              )
                                     )
                              ),
                              column(2)
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
                                     shiny::HTML("<h5>There are four easy steps to use the GeoDisparities Mapper:
                                                  <br><br>
                                                  <ol>
                                                    <li>Pick a perinatal outcome (either a single indicator, or a racial disparity measure)</li>
                                                    <li>Pick a contextual indicator (either social, economic, or healthcare related)</li>
                                                    <li>(Optionally) Pick one or more states to zoom in (alternatively, view the entire U.S.)</li>
                                                    <li>Review the geographic pattern (two maps at the top) and the statistical relationship (graphs at the bottom)</li>
                                                    </ol>
                                                    <br><br>
                                                    Go to the GeoDisparities Mapper now or continue reading for more information.
                                                 </h5>")
                              ),
                              column(3)
                            ),
                            
                            fluidRow(
                              column(3),
                              column(6,
                                     tags$div(class="wrap",
                                              #technical details button
                                              div(class = "center", 
                                                  style="display: inline-block;vertical-align:top; width: 175px;",
                                                  tags$a("Go to the GeoDisparites Mapper now!",
                                                         onclick = "fakeClick('geomapper')",
                                                         class="btn btn-primary btn-lg")
                                              )
                                        )      
                                     ),
                              column(3)
                              
                            ),
                          
                            # PAGE BREAK
                            tags$hr(),
                            
                            # ---------------- in more depth: place matters
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<center> <h2>In more depth...</h2> </center><br>"),
                                     shiny::HTML("<left> <h4><b><em>Place matters</em></b></h4> </left><br>"),
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
                                                    than average risk, there are still differences between counties. Why do these differences exist? 
                                                    In some instances it may be because of demographic profiles of counties, and in others it may 
                                                    be about the social, economic, and health service profiles of counties.</li>
                                                    </ul>
                                                 </h5>"),
                                     #Place Matters for Perinatal Health map figure
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 1: Preterm birth estimates in U.S. counties, 2017"),
                                               tags$img(src = "tutorial_fig1_placemattersforperinatalhealth.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ), #end place matters for perinatal health map div
                                     
                                     shiny::HTML("<h5> 
                                                  <ul>
                                                  <li><u>What do the colors on the map mean?</u> All of the maps in the 
                                                  GeoDisparities Mapper show the average risk for
                                                  perinatal outcomes in each county, with darker colors representing
                                                  <strong><em>higher risk</em></strong> and lighter colors representing
                                                  <strong><em>lower risk</em></strong>. To assign a map color to each county, the
                                                  counties are ranked from lowest to highest, and then categorized into 5 groups,
                                                  each with an equal number of counties (‘quintiles’). This makes it easy to see
                                                  the <em>relative ordering</em> of counties, but it does not always tell us how
                                                  big the absolute differences are between color categories. To see the
                                                  <em>absolute</em> risk associated with each color, look at the map legend.</li>
                                                  <br>
                                                  <li>Place also matters for community-level risk factors for poor health
                                                  outcomes. In this map, counties without access to maternity health care services
                                                  (‘Maternity Care Deserts’) are evident in nearly every state:</li>
                                                  <ul>
                                                 </h5>"),
                                     #maternity care desert map figure
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 2: Access to maternity care in U.S. counties, 2016 
                                                       (", a(href="https://www.marchofdimes.org/materials/Nowhere_to_Go_Final.pdf","Source"), ")"),
                                               tags$img(src = "tutorial_fig2_MoDmaternitycaredeserts.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ), #end maternity care desert map div
                                     
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li>The GeoDisparities Mapper includes dozens of community-level indicators of 
                                                  risk and resilience that are social, economic, and health service related. Each 
                                                  of them were selected because they are plausibly risk factors for between-county 
                                                  differences in risk for poor perinatal outcomes, and because they vary geographically.</li>
                                                  <br>
                                                  <li>While the GeoDisparities Mapper can only show correlation, but does not prove 
                                                  causation, the variables selected are nonetheless useful and important measures 
                                                  for asking better questions and engaging local partners in better understanding 
                                                  the local drivers of health and equity.</li>
                                                  </ul>
                                                 </h5>")
                              ),
                              column(3)
                            ), # ends in more depth: place matters
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # ---------------- in more depth: outcome matters
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<left> <h4><b><em>Outcomes matters</b></em></h4> </left><br>"),
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li>Preterm birth is not just one thing, but is really the outcome from a mix of many 
                                                  different disease processes (it is a ‘heterogeneous outcome’)! What this means is that 
                                                  the causes (and solutions) are not the same for every baby born preterm. It is difficult 
                                                  to account for all of these differences using only birth certificate data. But one way
                                                  to begin to see differences is by looking at the severity of prematurity. For instance 
                                                  babies born at <32 weeks gestation (sometimes called ‘very preterm birth’) have much 
                                                  higher risk for morbidity and mortality than babies born just one week before 37 weeks 
                                                  of gestation (more generally called ‘preterm birth’). Not only are the consequences 
                                                  different, but in some instances the causes or reasons for prematurity may be different. 
                                                  For instance, it is very unlikely that any baby would be intentionally delivered (e.g. 
                                                  labor induction or cesarean section) before 32 weeks except in the most serious of 
                                                  situations. But it is much more common for obstetric decision making about labor 
                                                  induction or cesarean section to play a role in late preterm births. Therefore the 
                                                  underlying drivers of each may be different (e.g. maternal health or chronic stress 
                                                  may play a relatively larger role in the case of very preterm birth; obstetric management 
                                                  and health care practices playing a larger role in preterm birth).
                                                  </li>
                                                  <br>
                                                  <li>In the GeoDisparities Mapper we provide indicators for the live birth prevalence 
                                                  (‘risk’) of overall preterm birth (<37 weeks gestation) but also several other 
                                                  subsets including the proportion of babies born <32 weeks (very preterm birth); 
                                                  34-36 weeks (late preterm birth); and, recognizing recent evidence of differences among 
                                                  term babies, even 37-39 weeks (early term).
                                                  </li>
                                                  </ul>
                                                 </h5>"),
                                     
                                     #gestational age categories figure 
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 3: Categories of gestational age"),
                                               tags$img(src = "tutorial_fig3_gestationalagecategories.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ), #gestational age categories figure div
                                     
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li><u>Are there differences in the geographic pattern of different perinatal outcomes?</u> 
                                                  In the plot below we show how the risk for preterm birth (<37 weeks) correlates with 
                                                  the risk for very preterm birth (<32 weeks) <em>in the very same counties.</em>  Obviously the 
                                                  absolute risk is different between these two outcomes, but we do see that they are 
                                                  correlated. As the risk for one goes up in a county, we tend to see the risk of the 
                                                  other go up.  But one thing that is notable is that the two outcomes are not <em>perfectly 
                                                  correlated.</em> In other words One way to see how these outcomes might have different 
                                                  patterns is to examine the relationship between the county-level risk of Preterm Birth
                                                  (<37 weeks) and the risk in the same counties of Very Preterm Birth (<32 weeks). 
                                                  Obviously, very preterm birth (<32 weeks) occur far less frequently than overall preterm
                                                  births (<37 weeks). In this plot, there is clearly a correlation in the rates of these 
                                                  two related outcomes for both Black and White women. However, it is also clear that the 
                                                  two rates do not correlate perfectly. In other words some counties are relatively higher
                                                  for Preterm Birth but relatively lower for Very Preterm Birth. This pattern is especially
                                                  evident for Black mothers. 
                                                  </li>
                                                  </ul>
                                                 </h5>"),
                              ), #end column
                              column(3)
                            ),
                            
                            fluidRow(
                              column(3),
                              column(6, 
                                     # very preterm birth 
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 4: Association of perinatal outcomes within U.S. counties, 2017"),
                                               tags$img(src = "tutorial_fig4_vptb.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ), #gestational age categories figure div
                              ), # end vptb mothers column
                              
                              column(3)
                            ),
                            
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li>These differences can also be seen on maps. In each of the maps below, the colors indicate
                                                  the relative ordering of rates of each outcome. In the two maps below of the risk for <em>Very</em> 
                                                  Preterm birth and <em>Preterm</em> Birth among non-Hispanic Black women, we can see that counties 
                                                  along the Atlantic coastal states have <em>relatively lower</em> Preterm Birth, but <em>relatively higher</em> 
                                                  Very Preterm Birth. The opposite pattern is evident in Southeastern Texas counties. These 
                                                  differences may point to different risk profiles in these counties.
                                                  </li>
                                                  <br>
                                                  <li><u>How can we compare the colors in two maps side-by-side?</u> As discussed above, the 
                                                  GeoDisparities Mapper uses a mapping rule that ranks all counties in a single maps from the 
                                                  lowest to the highest risk, and then groups them into 5 equally sized categories (‘quintiles’).  
                                                  The colors are assigned to these categories from the lowest (lighter color) to highest 
                                                  (darker color).   If two maps are side by side, the colors in one map do not necessarily 
                                                  reflect the same absolute risk as that color in another  map. It is always important to look
                                                  at the map legend to understand what the colors represent.
                                                  </li>
                                                  </ul>
                                                 </h5>"),
                                     
                                     #comparing colors in maps figure
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 5: Comparing geographic patterns of different perinatal outcomes, Black women, 2017"),
                                               tags$img(src = "tutorial_fig5_comparecolorsinmaps.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ), #gestational age categories figure div
                                     
                              ),
                              column(3)
                            ),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<left> <h4><b><em>Race matters</b></em></h4> </left><br>"),
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li>The notion that ‘place matters for health’ suggests that some places are more or less 
                                                  healthy for <em>everyone in the county.</em> Many aspects of community context likely do affect 
                                                  everyone living in that community. For example, counties with no maternity health care 
                                                  services means lower access for all. However, because of the history of slavery, Jim Crow 
                                                  laws, economic inequality, racial residential segregation, and other processes of 
                                                  structural racism and inequality, there can be differences in <em>where</em> Black and White 
                                                  families live, but also differences in <em>how</em> Black and White families experience even the
                                                  same places in which they live.
                                                  </li>
                                                  <br>
                                                  <li>For example, there can be place-based racial disparities in educational and economic
                                                  opportunity, racial differences in access to and quality of health care, and differences 
                                                  in experience of chronic stressors known to be risk factors for preterm birth.  These 
                                                  differences can produce <em>racial differences</em> in the geographic patterns of relatively 
                                                  higher or relatively lower risk for perinatal outcomes.
                                                  </li>
                                                  <br>
                                                  <li>There may be distinct geographic patterns of perinatal outcomes for the total population,
                                                  and  separate patterns for groups defined by race/ethnicty, or even when viewing measures of
                                                  health equity between race groups (link to Measures Matter, below).
                                                  </li>
                                                  <br>
                                                  <li>For example in this map of very low birth weight, the region with the highest relative 
                                                  risk for White women is the Appalachian Mountain region, and generally ‘moderate’ levels 
                                                  of risk in the Deep South. On the other hand, for Black women, the largest concentration 
                                                  of high risk are in the Deep South.
                                                  </li>
                                                  </ul>
                                                  </h5>"
                                     ),
                                     #very low birthweight among white women in appalachia & black women in deep south maps figure
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 6: Comparing geographic patterns in preterm birth by maternal race, 2017"),
                                               tags$img(src = "tutorial_fig6_geopatternsmaternalrace.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ), #end vlbw map figure
                                     
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li>The GeoDisparities Mapper currently includes perinatal outcome indicators for 
                                                  non-Hispanic White, non-Hispanic Black, and Hispanic women in counties where there
                                                  are sufficient births to each group to produce a statistically reliable estimate. 
                                                  When there are not sufficient births to estimate a reliable value, the counties are 
                                                  colored grey.
                                                  </li>
                                                  </ul>
                                                 </h5>")
                              ),
                              column(3)
                            ),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            #measures matter
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<left> <h4><b><em>Measures matters</b></em></h4> </left><br>"),
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li>Numbers don’t lie but a single number does not always tell the whole truth. 
                                                  Take, for instance, measures of health equity. In the United States, on average 
                                                  Black women and infants experience excess burden of poor perinatal outcomes as 
                                                  compared to non-Hispanic White women. As discussed above, there is little 
                                                  evidence that genetics or individual health behaviors explain this difference. 
                                                  Instead, many researchers posit that the reason for the persistence of racial 
                                                  inequity in perinatal outcomes is due to historical inequality by race and 
                                                  socioeconomic opportunity, factors that not only affect pregnancy, but can 
                                                  influence girls and women’s health across their life and even across generations. 
                                                  </li>
                                                  <br>
                                                  <li>In the United States it is quite common to ‘measure’ health disparities 
                                                  (health inequity) between two groups by dividing the risk of one group by the
                                                  risk of another. We may call this measure a <em>risk ratio.</em> The interpretation of
                                                  a <em>risk ratio</em> of 1.0 is that both groups have equal risk for the outcome. 
                                                  In contrast a risk ratio of 1.5 and 2.0 mean that the high-risk group has either 
                                                  50% increased risk or 100% increased risk (double the risk) as compared to the 
                                                  lower risk group. You may have seen these measures used for describing racial 
                                                  disparities in perinatal outcomes.
                                                  </li>
                                                  <br>
                                                  <li>But there are other ways to monitor population health equity. One other
                                                  measure is a <em>risk difference</em> rather than ratio. Instead of dividing 
                                                  the risk in one group by the other, a <em>risk difference</em> subtracts the 
                                                  risk in one group from another (e.g. takes the absolute arithmetic difference). 
                                                  Why would this tell us anything different from the <em>risk ratio?</em> Because
                                                  the same <em>absolute</em>  difference in risk will produce a different 
                                                  <em>relative</em> (ratio) gap when the lower-risk group is doing better or worse.
                                                  </li>
                                                  <br>
                                                  <li>In the figure below, we plot the Black-White <em>rate ratio</em> for U.S. 
                                                  counties against the Black-White <em>risk difference</em> for the very same 
                                                  counties. Clearly the two values are generally correlated (go up together). 
                                                  But looking at counties along the purple line (representing a three-fold higher
                                                  risk of very preterm birth among Black women as compared to White women), we can
                                                  see that there are a range of Black-White <em>risk differences.</em> The colors 
                                                  in this plot demonstrate how much the risk for very preterm birth in each county
                                                  among the lower risk group (non-Hispanic White women) varies. In counties where 
                                                  White women have low risk (colored blue), and a Black-White <em>risk ratio</em>
                                                  of 3.0, the Black-White <em>risk difference</em> is low (between 11 and 13 per 
                                                  1,000 births). But if we look at counties where White women have higher risk 
                                                  (colored red), the very same Black-White <em>risk ratio</em> of 3.0 results in 
                                                  a Black-White <em>risk difference</em> of 15-18.  
                                                  </li>
                                                  <br>
                                                  </ul>
                                                 </h5>"),
                                     #Different Measures of Health Equity Tell Different Stories figure
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 7: Comparing racial disparities measured as 'risk ratios' and 'risk differences' for preterm birth, 2017"),
                                               tags$img(src = "tutorial_fig7_diffmeasureshealthequity.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ),
                                     shiny::HTML("<h5>
                                           <ul>
                                           <li><u>Why does this happen?</u>  One way to understand is to imagine two different 
                                           counties. In County A, non-Hispanic White women have a 1% risk for very preterm birth 
                                           (<32 weeks), while non-Hispanic Black women have a 3% risk.  In County B, non-Hispanic
                                           White women have a risk of 2% and non-Hispanic Black women have a 4% risk.   It is clear
                                           that in each county Black women have a risk for very preterm birth that is 2% higher 
                                           in absolute terms. In other words, the Black-White disparity measured with an <em>absolute
                                           risk difference</em> is 2 excess very preterm births per 100 births. However, if we measured
                                           the disparity with the <em>relative risk ratio,</em> in County A, Black women have three-times 
                                           (<em>risk ratio 3% / 1% = 3.0</em>) the risk for very preterm birth. In County B, Black 
                                           women have two-times the risk (4% / 2% = 2.0). 
                                           </li>
                                           <br>
                                           <li>Once again, these differences in <em>how big the racial disparity is</em> can be seen on maps.
                                           </li>
                                           <br>
                                           </ul>
                                          </h5>"),
                                     #Geography of Health Inequity figure
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5("Figure 8: Comparing geographic patterns of Black-White racial disparities by choice of measure, 2017"),
                                               tags$img(src = "tutorial_fig8_geohealthinequity.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ),
                                     shiny::HTML("<h5>
                                           <ul>
                                           <li>In this map it appears that the Black-White racial disparity in the some Deep South counties
                                           is relatively <u><em>larger</em></u> when using the <em>absolute risk difference</em> measure, but
                                           in those same counties the Black-White racial disparity is relatively <u><em>smaller</em></u>
                                           when using the <em>relative risk ratio</em> measure. This is because the risk for very preterm
                                           birth is higher for <em>both Black and White</em> women in Deep Southern counties as compared 
                                           to many counties in the Northeast.
                                           </li>
                                           <br>
                                           <li><u>What do these measures tell us?</u> Both measures are mathematically correct. However 
                                           they tell slightly different stories about where disparities in perinatal outcomes are larger
                                           or smaller.  Making maps of health equity can be a useful complement to maps of only risk in 
                                           the whole population or for individual racial or ethnic groups. However it is important to 
                                           consider whether a large or small disparity is because both groups are doing well or both 
                                           are doing poorly.
                                           </li>
                                           <br>
                                           </ul>
                                          </h5>")
                                     
                              ),
                              
                              column(3)
                            )
                            
                   ),  # Closes tutorial tab
                   
                   #################################################################################################  
                   # -------------------------- "PERINATAL VARIABLES" PAGE ------------------------------------------- 
                   #################################################################################################  
                   
                   tabPanel("Perinatal Variables", value = "perinatalpage",
                            
                            fluidRow(
                              shiny::HTML("<br><br><center> 
                                                   <h1>GeoDisparities Mapper: Perinatal Outcome Variables</h1> 
                                                   <h4>What's behind the modeled data.</h4>
                                                   </center>
                                                   <br>
                                                   <br>"),
                              style = "height:250px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # ---------------- in brief.....
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<center> <h2>In brief...</h2> </center><br>"),
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li><u>Why are the perinatal indicators in the GeoDisparities Mapper only 
                                                  estimates rather than the actual risk?</u>
                                                  </li>
                                                  <br>
                                                  <li>It is true that birth certificate information on nearly every live birth in 
                                                  the United States is collected by the National Center for Health Statistics (NCHS).
                                                  These records include the gestational age and weight of the infant at birth, 
                                                  information about the mother and the county in which she lived at the time of 
                                                  birth. So wouldn’t it be better to just map the <u><em>“actual” risk</em></u> 
                                                  rather than an <u><em>estimate?</u></em> We believe the answer is ‘no’ for two reasons. 
                                                  <br>
                                                  <ol>
                                                  <li>The first is that the NCHS does not publicly distribute the county of residence 
                                                  for vital records to protect the privacy of individual women and small demographic 
                                                  sub-groups. These data are available under a special Data Use Agreement with NCHS, 
                                                  but only for specific, targeted use by individuals who are part of the Agreement.
                                                  </li>
                                                  <br>
                                                  <li>The second reason is that, even if we could share the actual count of preterm 
                                                  births for every county, the statistics would be considered <u><em>“unreliable”</em></u>
                                                  or <u><em>“unstable”</em></u> for many counties where there are not very many births 
                                                  to the specific race/ethnic group being mapped.  Imagine there are two counties, each 
                                                  measured for three separate years. The number of live births stayed the same in each
                                                  county for all three years. However each year, perhaps due to random chance, there 
                                                  was one additional baby born preterm than the previous year.  In County B, the risk 
                                                  of preterm birth is very <u><em>stable</em></u> around 10%. In other words the 
                                                  relatively random fluctuation of one or two more or fewer preterm babies did not 
                                                  change the estimate of risk very much. In contrast, in County B the risk ranged from
                                                  0% to 20%, both very extreme values for preterm birth. This is because there are not
                                                  many births at risk in County A. When there are a very small number of births or 
                                                  very few poor outcomes (e.g. ‘sparse data’), the calculated risk can be statistically
                                                  unreliable or unstable, and can easily take on unreasonably extreme values.
                                                  </li>
                                                  </ol>
                                                  </li>
                                                  <br>
                                                  </ul>
                                                 </h5>"),
                                     #table
                                     div(class="panel panel-default", 
                                         div(class="panel-body",  width = "800px",
                                             align = "center",
                                             div(
                                               tags$h5(""),
                                               tags$img(src = "perinatal_table.png", 
                                                        width = "750px")#, height = "50px")
                                             )
                                         )
                                     ),
                                     
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li><u>What does it mean to ‘estimate’ risk and how was it done here?</u>
                                                  </li>
                                                  <br>
                                                  <li>Risk estimates are an effort to produce the most reliable prediction of the
                                                  underlying ‘true’ risk of poor perinatal outcomes experienced by a group of women
                                                  (e.g. in a specific race/ethnic group in a specific county).  To the extent that 
                                                  the methods and approach used to calculate the estimate are valid and robust, the
                                                  resulting estimate is more useful for comparing across counties or across years. 
                                                  In other words the estimation process aims to describe what is really different 
                                                  about risk without too many extreme swings that are due only to sparse data and random
                                                  chance.
                                                  </li>
                                                  <br>
                                                  <li>The approach to <em>perinatal risk estimation</em> in the GeoDisparities Mapper 
                                                  uses sophisticated spatio-temporal statistical models called Bayesian disease mapping.
                                                  The more technical details of the modeling procedure are described below. However in 
                                                  broad terms, it is well known that, on average, the risk in one year is more correlated
                                                  with the risk in the previous year than it is with risk much longer ago. Similarly, on 
                                                  average, the risk in one county is more similar to the risk in nearby counties than it
                                                  is to a randomly selected, distant county. Of course there are exceptions to both 
                                                  situations. However the Bayesian disease mapping approach is a way to stabilize risk 
                                                  estimates from sparse populations (counties or race/ethnic groups) by ‘borrowing 
                                                  statistical information’ about risk from the previous year(s) and from the nearby 
                                                  counties. This approach has been widely used by CDC, NCHS, and the World Health 
                                                  Organization to develop statistically reliable maps of geographic patterns of population health.
                                                  </li>
                                                  <br>
                                                  </ul>
                                                 </h5>")
                              ),
                              column(3)
                            ),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<center> <h2>Technical detail of risk estimation models</h2> </center><br>"),
                                     shiny::HTML("<h5>
                                                  <ul>
                                                  <li>The specific model selection process is still underway. Once complete we 
                                                  will population this section with the statistical detail, but also with some 
                                                  evidence about model fit and performance. 
                                                  </li>
                                                  </ul>
                                                 </h5>")
                              ),
                              column(3)
                            ),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
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
                                              # introBox(
                                              #   tags$div(
                                              #     style = "height:50px;",
                                              #     actionLink("settings", "Settings", 
                                              #                icon = icon("sliders", class = "fa-2x"))),
                                              #   data.step = 6, #data steps enumerated
                                              #   data.intro = "Settings is where you can set options that affect the visualization of data as maps and plots."
                                              # ),
                                              
                                              #select geography
                                              introBox(
                                                selectizeInput("state", "Select your geography",
                                                               choices = c(state_names),
                                                               selected = "Georgia",
                                                               multiple = TRUE),
                                                data.step = 1,
                                                data.intro = "Start by picking one or more states to zoom in (alternatively, view the entire U.S."
                                                
                                              ),
                                              
                                              
                                              #select model data variables data to be mapped & plotted
                                              introBox(
                                                uiOutput('modvar'),
                                                data.step = 2,
                                                data.intro = "Pick a perinatal outcome (either a single indicator, or a racial disparity measure)"
                                                
                                              ),
                                              
                                              #select contextual variables data to be mapped & plotted
                                              introBox(
                                                uiOutput('contextvar'),
                                                data.step = 3,
                                                data.intro = "Pick a contextual indicator (either social, economic, or healthcare related)"
                                              ),
                                              
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
                                              
                                              #select number of quantiles for right-side data
                                              sliderInput('modquantiles', label = "Perinatal Outcome Variable Quantiles", 
                                                          value = 5, min = 3, max = 7, step = 1, sep = ""
                                              ),
                                              
                                              #select number of quantiles for left-side data
                                              sliderInput('contextquantiles', label = "Contextual Variable Quantiles", 
                                                          value = 5, min = 3, max = 7, step = 1, sep = ""
                                              ),
                                              
                                              
                                              
                                              #select year
                                              sliderInput('year', label = "Year", value = min(context_data$year), 
                                                          min = min(context_data$year), max = max(context_data$year), 
                                                          step=1, sep = ""
                                                          #,animate = animationOptions(interval = 750) #too slow with animation
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
                                                  tags$h3("Perinatal Outcome Variable")
                                           ),
                                           column(6,
                                                  tags$h3("Contextual Variable")
                                           )
                                           
                                         ),
                                         
                                         # PAGE BREAK
                                         tags$hr(),
                                         
                                         #Title for maps section
                                         introBox(
                                             fluidRow(
                                               column(12,
                                                      tags$h4("Maps"),
                                                      align="center"
                                               ),
                                               column(6,
                                                      tags$em(tags$h5("What does the geographic distribution of this perinatal output variable tell you?")),
                                                      align="center"
                                               ),
                                               
                                               column(6,
                                                      tags$em(tags$h5("What does the geographic distribution of this contextual variable tell you?")),
                                                      align="center"
                                               )
                                               
                                              ),
                                         data.step = 4,
                                         data.intro = "Review the geographic patterns in the two maps below"
                                         ),
                                         
                                         # maps
                                         fluidRow(
                                           
                                           column(6,
                                                  leafletOutput("modmap")
                                           ),
                                           
                                           column(6,
                                                  leafletOutput("contextmap")
                                           )
                                           
                                         ),
                                         
                                         fluidRow(
                                           
                                           style = "height:50px;"
                                         ),
                                         
                                         # PAGE BREAK
                                         tags$hr(),
                                         
                                         #Title for univariate plots section
                                         introBox(
                                             fluidRow(
                                               column(12,
                                                      tags$h4("Univariate Plots"),
                                                      align="center"
                                               ),
                                               
                                               
                                               column(6,
                                                      tags$em(tags$h5("How does this perinatal variable vary?")),
                                                      align="center"
                                               ),
                                               column(6,
                                                      tags$em(tags$h5("How does this contextual variable vary?")),
                                                      align="center"
                                               )
                                               
                                              ),
                                         data.step = 5,
                                         data.intro = "Review the statistical relationships visualized in the graphs below"
                                         ),
                                         
                                         #Univariate scatter plots
                                         fluidRow(
                                           
                                           column(6,
                                                  plotOutput("modscatter")
                                           ),
                                           column(6,
                                                  plotOutput("contextscatter")
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
                                                  tags$em(tags$h5(("What does the relationship between the contextual and the perinatal outcome variables signify?"))),
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