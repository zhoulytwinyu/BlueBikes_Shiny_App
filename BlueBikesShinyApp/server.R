library(shiny)
library(htmlwidgets)
library(leaflet)
library(plotly)

library(pool)
library(htmltools)
####################
# Define server logic required to draw a histogram
####################

server <- function(input, output,session) {
  ####################
  # Tab: Interactive map
  ####################
  output$bostonMap <- renderLeaflet({
    stations_df <- pool %>% dbReadTable("stations")

    m <- leaflet() %>%
            setView(lng = BOSTON_LON, lat = BOSTON_LAT, zoom = 11) %>%
            addProviderTiles(providers$Stamen.TonerLite) %>%
            addMarkers( lng = ~longitude, lat = ~latitude,
                        layerId = ~name,
                        popup = ~htmlEscape(name),
                        data = stations_df
                        )
    return(m)
  })

  # Adjust in-map circle size based on input ranges
  observe({
    map <- leafletProxy("bostonMap")
    dateRange <- input$dateSlide
    hourRange <- input$hourSlide
    scale <- input$scaleSlide/10
    sqlTemplate <- paste(
      "SELECT latitude, longitude, name, influx, eflux",
      "FROM stations",
      "  JOIN (SELECT start_station_id, COUNT(*) as eflux",
      "        FROM trips",
      "        WHERE startdate >= ?dateMin AND startdate <= ?dateMax AND",
      "              starttime >= ?hourMin AND starttime <= ?hourMax",
      "        GROUP BY start_station_id",
      "        ) efluxTable ON efluxTable.start_station_id = stations.id",
      "  JOIN (SELECT end_station_id, COUNT(*) as influx",
      "        FROM trips",
      "        WHERE startdate >= ?dateMin AND startdate <= ?dateMax AND",
      "              starttime >= ?hourMin AND starttime <= ?hourMax",
      "        GROUP BY end_station_id",
      "        ) influxTable ON influxTable.end_station_id = stations.id"
                         )
    sql <- sqlInterpolate(ANSI(), sqlTemplate,
                          dateMin=dateRange[1], dateMax=dateRange[2],
                          hourMin=hourRange[1], hourMax=hourRange[2])
    stations_df <- pool %>% dbGetQuery(sql)
    map %>% addCircles(lng = ~longitude, lat = ~latitude,
                       layerId = ~paste(name,"influx"),
                       data = stations_df,
                       radius = ~scale*influx, color = "firebrick",
                       stroke=FALSE, fillOpacity = 0.5) %>%
            addCircles(lng = ~longitude, lat = ~latitude,
                       layerId = ~paste(name,"eflux"),
                       data = stations_df,
                       radius = ~scale*eflux, color = "royalblue",
                       stroke=FALSE, fillOpacity = 0.5)
    map %>% addLegend("bottomright",
                      layerId="legend",
                      title="Legend",
                      colors=c("firebrick","royalblue"), labels=c("influx","eflux")
                      )
  })

  output$stationName <- renderText({
    mapSelection <- input$bostonMap_marker_click
    return(mapSelection[["id"]])
  })
  # Station plots
  output$stationDailyUsagePlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    if (is.null(station)){
      return(plotly_empty(type="scatter", mode="lines"))
    }
    sqlTemplate <- paste(
      "SELECT startdate, ",
      "       SUM(CASE start_station.name WHEN ?name THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE end_station.name WHEN ?name THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY startdate "
                        )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, name=station)
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~startdate) %>%
          add_trace(y = ~eflux, name = "eflux", type="scatter", mode = "lines") %>%
          add_trace(y = ~influx, name = "influx", type="scatter", mode = "lines") %>%
          layout(xaxis = list(title="day of the month"),
                 yaxis = list(title="trips/day")
                )
    return(p)
  })

  output$stationHourlyUsagePlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    if (is.null(station)){
      return(plotly_empty(type="scatter", mode="lines"))
    }
    sqlTemplate <- paste(
      "SELECT starttime, ",
      "       SUM(CASE start_station.name WHEN ?name THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE end_station.name WHEN ?name THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY starttime "
              )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, name=station)
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~starttime) %>%
          add_trace(y = ~eflux, name = "eflux", type="scatter", mode = "lines") %>%
          add_trace(y = ~influx, name = "influx", type="scatter", mode = "lines") %>%
          layout(xaxis = list(title="hour of day"),
                 yaxis = list(title="total trips/hour")
                )
    return(p)
  })

  output$stationUserAgePlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    if (is.null(station)){
      return(plotly_empty(type="bar"))
    }
    sqlTemplate <- paste(
      "SELECT 2018-birth_year/5*5 AS age, ",
      "       SUM(CASE start_station.name WHEN ?name THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE end_station.name WHEN ?name THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY age "
              )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, name=station)
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~age) %>%
          add_trace(y = ~eflux, name = "eflux", type="bar") %>%
          add_trace(y = ~influx, name = "influx", type="bar") %>%
          layout(xaxis = list(title="age", range=c(0,100)),
                 yaxis = list(title="trips"),
                 barmode = "stack"
                )
    return(p)
  })

  output$stationTripLengthPlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    if (is.null(station)){
      return(plotly_empty(type="scatter", mode="lines"))
    }
    sqlTemplate <- paste(
      "SELECT tripduration/60/5*5 as duration, ",
      "       SUM(CASE start_station.name WHEN ?name THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE end_station.name WHEN ?name THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY duration "
              )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, name=station)
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~duration) %>%
          add_trace(y = ~eflux, name = "eflux", type="bar") %>%
          add_trace(y = ~influx, name = "influx", type="bar") %>%
          layout(xaxis = list(title="trip duration (min)",range=c(0,120)),
                 yaxis = list(title="trips", type="log")
                )
    return(p)
  })

  # Pair plots
  output$pairDailyUsagePlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    dest <- input$bostonMap_marker_mouseover[["id"]]
    if (is.null(station) || is.null(dest) ) {
      return(plotly_empty(type="scatter", mode="lines"))
    }
    sqlTemplate <- paste(
      "SELECT startdate, ",
      "       SUM(CASE WHEN start_station.name = ?nameS AND end_station.name = ?nameE THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE WHEN start_station.name = ?nameE AND end_station.name = ?nameS THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY startdate "
              )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, nameS=station, nameE=dest )
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~startdate) %>%
          add_trace(y = ~eflux, name = "eflux", type="scatter", mode = "lines") %>%
          add_trace(y = ~influx, name = "influx", type="scatter", mode = "lines") %>%
          layout(xaxis = list(title="day of the month"),
                 yaxis = list(title="trips/day")
                )
    return(p)
  })

  output$pairHourlyUsagePlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    dest <- input$bostonMap_marker_mouseover[["id"]]
    if (is.null(station) || is.null(dest) ) {
      return(plotly_empty(type="scatter", mode="lines"))
    }
    sqlTemplate <- paste(
      "SELECT starttime, ",
      "       SUM(CASE WHEN start_station.name = ?nameS AND end_station.name = ?nameE THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE WHEN start_station.name = ?nameE AND end_station.name = ?nameS THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY starttime "
              )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, nameS=station, nameE=dest )
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~starttime) %>%
          add_trace(y = ~eflux, name = "eflux", type="scatter", mode = "lines") %>%
          add_trace(y = ~influx, name = "influx", type="scatter", mode = "lines") %>%
          layout(xaxis = list(title="hour of day"),
                 yaxis = list(title="total trips/hour")
                )
    return(p)
  })

  output$pairUserAgePlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    dest <- input$bostonMap_marker_mouseover[["id"]]
    if (is.null(station) || is.null(dest) ) {
      return(plotly_empty(type="scatter", mode="lines"))
    }
    sqlTemplate <- paste(
      "SELECT 2018-birth_year/5*5 AS age, ",
      "       SUM(CASE WHEN start_station.name = ?nameS AND end_station.name = ?nameE THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE WHEN start_station.name = ?nameE AND end_station.name = ?nameS THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY age "
              )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, nameS=station, nameE=dest )
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~age) %>%
          add_trace(y = ~eflux, name = "eflux", type="bar") %>%
          add_trace(y = ~influx, name = "influx", type="bar") %>%
          layout(xaxis = list(title="age", range=c(0,100)),
                 yaxis = list(title="trips"),
                 barmode = "stack"
                )
    return(p)
  })

  output$pairTripLengthPlot <- renderPlotly({
    station <- input$bostonMap_marker_click[["id"]]
    dest <- input$bostonMap_marker_mouseover[["id"]]
    if (is.null(station) || is.null(dest) ) {
      return(plotly_empty(type="scatter", mode="lines"))
    }
    sqlTemplate <- paste(
      "SELECT tripduration/60/5*5 as duration, ",
      "       SUM(CASE WHEN start_station.name = ?nameS AND end_station.name = ?nameE THEN 1 ELSE 0 END) AS eflux, ",
      "       SUM(CASE WHEN start_station.name = ?nameE AND end_station.name = ?nameS THEN 1 ELSE 0 END) AS influx ",
      "FROM trips ",
      "  JOIN stations AS start_station ON start_station.id = trips.start_station_id ",
      "  JOIN stations AS end_station ON end_station.id = trips.end_station_id ",
      "GROUP BY duration "
              )
    sql <- sqlInterpolate(ANSI(), sqlTemplate, nameS=station, nameE=dest )
    trips_df <- pool %>% dbGetQuery(sql)

    p <- plot_ly(trips_df, x=~duration) %>%
          add_trace(y = ~eflux, name = "eflux", type="bar") %>%
          add_trace(y = ~influx, name = "influx", type="bar") %>%
          layout(xaxis = list(title="trip duration (min)",range=c(0,60)),
                 yaxis = list(title="trips", type="log")
                )
    return(p)
  })


  ####################
  # Tab: Summary
  ####################
  output$dailyUsagePlot <- renderPlotly({
    res <- pool %>% poolCheckout() %>%
                         dbSendQuery( paste(
                                      "SELECT startdate, COUNT(*) as usage ",
                                      "FROM trips ",
                                      "GROUP BY startdate "
                                      )
                                    )
    trips_df <- dbFetch(res)
    dbClearResult(res)

    p <- plot_ly(trips_df, x = ~startdate, y = ~usage,
                 name = 'usage', type="scatter", mode = 'lines'
                 ) %>%
         layout(xaxis = list(title="day of the month"),
                yaxis = list(title="trips/day")
                )
    return(p)
  })

  output$hourlyUsagePlot <- renderPlotly({
    res <- pool %>% poolCheckout() %>%
                         dbSendQuery( paste(
                                      "SELECT starttime, COUNT(*)/31.0 as usage ",
                                      "FROM trips ",
                                      "GROUP BY starttime "
                                      )
                                    )
    trips_df <- dbFetch(res)
    dbClearResult(res)

    p <- plot_ly(trips_df, x = ~starttime, y = ~usage,
                 type="scatter", mode = 'lines'
                 ) %>%
         layout(xaxis = list(title="hour of day"),
                yaxis = list(title="trips/hour")
                )
    return(p)
  })

  output$userAgePlot <- renderPlotly({
    res <- pool %>% poolCheckout() %>%
                         dbSendQuery( paste(
                                      "SELECT 2018-birth_year AS age ",
                                      "FROM trips "
                                      )
                                    )
    trips_df <- dbFetch(res)
    dbClearResult(res)

    p <- plot_ly(trips_df, x = ~age,
                 type = "histogram"
                 ) %>%
         layout(xaxis = list(title="age (in 2018)",range=c(0, 100) ),
                yaxis = list(title="trip count")
                )
    return(p)
  })

  output$tripLengthPlot <- renderPlotly({
    res <- pool %>% poolCheckout() %>%
                         dbSendQuery( paste(
                                      "SELECT tripduration/3600.0 AS duration ",
                                      "FROM trips "
                                      )
                                    )
    trips_df <- dbFetch(res)
    dbClearResult(res)

    p <- plot_ly(trips_df, x = ~duration,
                 type = "histogram"
                 ) %>%
         layout(xaxis = list(title="trip duration (hour)", range=c(0,24)),
                yaxis = list(title="trip count", type="log")
                )
    return(p)
  })

}
