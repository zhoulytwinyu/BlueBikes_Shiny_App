library(shiny)
library(leaflet)
library(plotly)

####################
## Define UI for application that draws a histogram
####################
ui <- tagList(
  #CSS/JS
  tags$head(tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "style.css"
                      ),
            tags$link(rel="icon",
                      type="image/x-icon",
                      href="favicon.ico?"
                      ),
            tags$script(src = "script.js"
                        )
            ),
  #PAGE
  navbarPage(
    # Title
    title = "Blue Bikes",
    selected = "Interactive Map",

    # Tab 1
    tabPanel("Interactive Map",
      sidebarLayout(
        sidebarPanel(
          # Slider buttons
          HTML('
            <button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#dateSlideDiv">Date&#9660</button>
            <button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#hourSlideDiv">Hour&#9660</button>
            <button class="btn btn-primary" type="button" data-toggle="collapse" data-target="#scaleSlideDiv">Scale&#9660</button>'
          ),
          # Sliders
          div(id="dateSlideDiv", class="collapse",
            sliderInput("dateSlide", label = HTML("<span class=\"h4\">Date</span> (Jul 1st-31st)"),
                        min = DATE_RANGE[1], max = DATE_RANGE[2], value = DATE_RANGE)
          ),
          div(id="hourSlideDiv", class="collapse",
            sliderInput("hourSlide", label = HTML("<span class=\"h4\">Hour</span>  (0-23)"),
              min = HOUR_RANGE[1], max = HOUR_RANGE[2], value = HOUR_RANGE)
          ),
          div(id="scaleSlideDiv", class="collapse",
            sliderInput("scaleSlide", label = HTML("<span class=\"h4\">Scale</span>  (0-2)"),
              min = 0.1, max = 2, value = 1)
          ),
          # When no station is selected
          conditionalPanel("input.bostonMap_marker_click == null",
            div(class="alert alert-info",role="alert",
              p("You can get statistics of a specific station."),
              strong("Click on one station.")
            )
          ),
          # When a station is selected
          conditionalPanel("input.bostonMap_marker_click != null",
            div(align="center",strong(textOutput("stationName"))),
            tabsetPanel(type = "tabs",
              tabPanel("Daily",
                conditionalPanel("input.bostonMap_marker_mouseover == null",
                  plotlyOutput("stationDailyUsagePlot", height="200px")
                ),
                conditionalPanel("input.bostonMap_marker_mouseover != null",
                  plotlyOutput("pairDailyUsagePlot", height="200px")
                )
              ),
              tabPanel("Hourly",
                conditionalPanel("input.bostonMap_marker_mouseover == null",
                  plotlyOutput("stationHourlyUsagePlot", height="200px")
                ),
                conditionalPanel("input.bostonMap_marker_mouseover != null",
                  plotlyOutput("pairHourlyUsagePlot", height="200px")
                )
              ),
              tabPanel("Age",
                conditionalPanel("input.bostonMap_marker_mouseover == null",
                  plotlyOutput("stationUserAgePlot", height="200px")
                ),
                conditionalPanel("input.bostonMap_marker_mouseover != null",
                  plotlyOutput("pairUserAgePlot", height="200px")
                )
              ),
              tabPanel("Length",
                conditionalPanel("input.bostonMap_marker_mouseover == null",
                  plotlyOutput("stationTripLengthPlot", height="200px")
                ),
                conditionalPanel("input.bostonMap_marker_mouseover != null",
                  plotlyOutput("pairTripLengthPlot", height="200px")
                )
              )
            )
          )
        ),
        # Map
        mainPanel(
          leafletOutput("bostonMap",height="600px")
        )
      )
    ),

    # Tab 2
    tabPanel("Summary",
      navlistPanel(
        "Blue Bikes Usage Summary (Jul 2018)",
        tabPanel("Daily Usage",
          plotlyOutput("dailyUsagePlot")
        ),
        tabPanel("Averaged Hourly Usage",
          plotlyOutput("hourlyUsagePlot")
        ),
        tabPanel("User Age Composition",
          plotlyOutput("userAgePlot")
        ),
        tabPanel("Trip Length",
          plotlyOutput("tripLengthPlot")
        )
      )
    )
  )
)
