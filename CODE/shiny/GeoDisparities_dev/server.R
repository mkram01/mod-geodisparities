##############################################
# Code author: erin r stearns
# Code objective: MoD GeoDisparities web tool - Server function - testing & building
# Date created: 8.15.2019
#############################################

# rsconnect 0.8.8

shinyServer(function(input, output, session) {

  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")

  # Start Button -------------------------------------------------------------
  observeEvent(input$startBtn, {
    updateNavbarPage(session, "navBar",
                     selected = "geomapper"
    )
  })

  # Intro JS ----------------------------------------------------------------
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back",
                                               "skipLabel"="Exit"))
  )

}) #close shiny server function
