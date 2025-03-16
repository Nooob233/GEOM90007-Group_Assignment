library("sf")
library("readxl")
library("shiny")
library("tidyverse")
library("tidyr")
library("ggplot2")
library("ggiraph")
library("ggbump")
library("leaflet")
library("leaflet.extras")
library("leafpop")
library("rnaturalearth")
library("data.tree")
library("readxl")
library("igraph")
library("networkD3")
library("DiagrammeR")
library("classInt")
library("data.table")
library("shinyjs")
library("httr")
library("jsonlite")
library("dplyr")
library("plotly")

#################
# Preprocessing #
#################
# Read data
df <- read_sf("data/microclimate-sensors-data_imputed.geojson")

df$lng <- st_coordinates(df$geometry)[, 1]
df$lat <- st_coordinates(df$geometry)[, 2]

# Create a df with only the location information
geom_df <- df %>%
  group_by(device_id) %>%
  filter(!is.na(sensorlocation)) %>%
  slice(1) %>%
  select(device_id, sensorlocation, geometry, lng, lat)

# Change saturation of blue to show the strength of wind
wind_speed_palette <- colorNumeric(
  palette = "Blues",
  domain = df$averagewindspeed,
  reverse = FALSE
)

humidity_palette <- colorNumeric(
  palette = "Blues",
  domain = df$relativehumidity,
  reverse = FALSE
)

df <- df %>% mutate(
  windStrengthColor = wind_speed_palette(averagewindspeed),
  humidityColor = humidity_palette(relativehumidity)
)

# Helper function to calculate wind cone
calculate_cone <- function(lon, lat, angle, length) {
  # Convert angle to radians for calculation
  angle_rad <- (angle - 90) * pi / 180

  # Calculate the tip of the cone
  delta_lon <- cos(angle_rad) * length / 111320 / cos(lat * pi / 180)
  delta_lat <- sin(angle_rad) * length / 110540

  tip_lon <- lon + delta_lon
  tip_lat <- lat + delta_lat

  # Calculate left and right base points for the cone at the starting location
  left_angle_rad <- (angle - 120) * pi / 180
  right_angle_rad <- (angle - 60) * pi / 180

  left_delta_lon <- cos(left_angle_rad) * (length / 4) / 111320 / cos(lat * pi / 180)
  left_delta_lat <- sin(left_angle_rad) * (length / 4) / 110540

  right_delta_lon <- cos(right_angle_rad) * (length / 4) / 111320 / cos(lat * pi / 180)
  right_delta_lat <- sin(right_angle_rad) * (length / 4) / 110540

  # Return coordinates for the polygon (cone)
  return(
    list(
      c(lon + left_delta_lon, tip_lon, lon + right_delta_lon),
      c(lat + left_delta_lat, tip_lat, lat + right_delta_lat)
    )
  )
}

# Helper function to format wind direction
format_wind_direction <- function(degrees) {
  if (is.na(degrees)) {
    return(NA)
  }

  directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  ranges <- seq(0, 360, by = 22.5)

  idx <- findInterval(degrees, ranges) %% length(directions) + 1
  cardinal <- directions[idx]

  return(paste0(round(degrees, 1), "° ", cardinal))
}

# Color palette for temperature
temperature_palette <- colorNumeric(
  palette = "RdBu",
  domain = c(-10, 30),
  reverse = TRUE
)

##################
# USER INTERFACE #
##################
climate_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script(src = "https://d3js.org/d3.v7.min.js"),
      tags$script(src = "scripts/compass.js"),
      tags$style(HTML("
        .full-height {
          height: calc(60vh);
          width: 100%;
        }
        .slider-container {
          display: flex;
          justify-content: center;
          align-items: center;
          gap: 80px;
          margin-top: 10px;
          margin-bottom: 10px;
        }
        .fixed-height {
          height: 10vh;
          width: 100%;
        }
      "))
    ),
    fluidRow(
      class = "fixed-height",
      div(
        class = "slider-container",
        selectInput(
          inputId = ns("measure"),
          label = "Measure",
          choices = c("Wind", "Air temperature", "Humidity", "Pressure", "Air quality", "Noise"),
          selected = "Wind"
        ),
        actionButton(ns("playButton"), "Play"),
        sliderInput(
          ns("timeSlider"), "Time", min(df$received_at), max(df$received_at),
          value = min(df$received_at), timeFormat = "%Y-%m-%d %H:%M:%S", step = 900,
        ),
        actionButton(ns("showDataButton"), "Show Latest Data")
      )
    ),
    fluidRow(
      div(
        class = "full-height",
        leafletOutput(ns("microclimateMap"), width = "100%", height = "100%")
      )
    ),
    fluidRow(
      div(
        class = "fixed-height",
        uiOutput(ns("tableauVizContainer"))
      )
    )
  )
}

################
# SHINY SERVER #
################
climate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    # Load the Tableau in Shiny functions
    setUpTableauInShiny()

    output$microclimateMap <- renderLeaflet({
      leaflet(df) %>%
        addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>%
        setView(lng = 144.9681, lat = -37.8156, zoom = 15) %>%
        addCircleMarkers(
          data = geom_df, lng = ~lng, lat = ~lat,
          layerId = ~device_id,
          radius = 1, color = "white", fillColor = "white", fillOpacity = 0.8
        )
    })

    # Reactive value for selected sensor
    selected_sensor <- reactiveVal(NULL)

    # Show Latest Data Dashboard
    observeEvent(input$showDataButton, {
      # Get the current selected device_id
      device_id <- selected_sensor()

      # Ensure a sensor is selected before proceeding
      if (is.null(device_id)) {
        device_id <- "ICTMicroclimate-01"
      }

      # URL for the API
      timestamp_24h_ago <- format(Sys.time() - hours(24), "%Y-%m-%dT%H:%M:%S")
      base_url <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/microclimate-sensors-data/records?"
      # retrieve all data from the last 24 hours
      url <- paste0(
        base_url,
        "select=*&where=device_id%20%3D%20%22", device_id,
        "%22%20AND%20received_at%20%3E%20%22", timestamp_24h_ago,
        "%22&order_by=received_at%20DESC"
      )

      # Fetch data from the API
      response <- GET(url)

      # Check for successful response
      if (status_code(response) == 200) {
        print("Latest API data successfulled loaded")
        # Parse the JSON response into an R object
        last24_data <- fromJSON(content(response, "text", encoding = "UTF-8"))$results
        
        print("last 24 hour data: ")
        print(last24_data)

        # Extract the latest data point (assuming data is a data frame)
        latest_data <- last24_data %>%
          as.data.frame() %>%
          slice(1)

        print("latest data:")
        print(latest_data)

        #--------------------------Wind---------------------------#
        direction <- latest_data$averagewinddirection
        direction_formatted <- format_wind_direction(direction)
        wind_speed <- latest_data$averagewindspeed

        # wind
        wind_content <- tags$div(
          style = "text-align: center;",
          # Left column with Wind Force, Gusts, and Direction
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; text-align: left;",
            tags$div(
              style = "background-color: #f0f0f0; padding: 0px; border-radius: 8px; display: flex; align-items: center;",

              # Left column with text details (flex: 1)
              tags$div(
                style = "flex: 1; padding-right: 10px; display: flex; flex-direction: column; align-items: flex-start;",
                tags$h5("Wind Force"),
                tags$p(tags$strong(latest_data$averagewindspeed, " bft")),
                tags$h5("Gusts"),
                tags$p(tags$strong(latest_data$gustwindspeed, " km/h")),
                tags$h5("Direction"),
                tags$p(tags$strong(direction_formatted)) # Example: "201° SSW"
              ),

              # Right column with the compass (flex: 1)
              tags$div(
                style = "flex: 1; display: flex; justify-content: center; align-items: center;",
                tags$div(
                  id = "windCompass",
                  style = "width: 100px; height: 100px; margin: 0;"
                )
              )
            )
          ),
        )

        # temperature
        output$temperatureGauge <- renderPlotly({
          # Ensure that last24_data is available and not empty
          req(last24_data)
          
          print(last24_data)

          # Calculate the temperature range and the most recent temperature
          min_temp <- min(last24_data$airtemperature, na.rm = TRUE)
          max_temp <- max(last24_data$airtemperature, na.rm = TRUE)
          recent_temp <- last24_data$airtemperature[1] # The most recent temperature


          # Create a gauge plot using plotly
          plot_ly(
            type = "indicator",
            mode = "gauge+number+delta",
            value = recent_temp,
            delta = list(reference = mean(c(min_temp, max_temp)), increasing = list(color = "red"), decreasing = list(color = "blue")),
            gauge = list(
              axis = list(range = list(min_temp, max_temp), tickwidth = 1, tickcolor = "darkblue"),
              bar = list(color = "black"),
              steps = lapply(seq(0, 1, length.out = 100), function(i) {
                list(
                  range = c(min_temp + i * (max_temp - min_temp), min_temp + (i + 0.01) * (max_temp - min_temp)),
                  color = colorRampPalette(c("lightblue", "lightcoral"))(100)[i * 100]
                )
              }),
              threshold = list(
                line = list(color = "black", width = 4),
                thickness = 0.75,
                value = recent_temp
              )
            ),
          ) %>%
            layout(
              paper_bgcolor = "#f0f0f0", # Background of the entire plot
              plot_bgcolor = "#f0f0f0" # Background of the gauge area
            )
        })

        # Prepare the content to display in the modal
        modal_content <- tags$div(
          h4("Latest Data for Device ID:", device_id),
          p(strong("Received At:"), latest_data$received_at),

          # container for the 6 measures
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; grid-template-rows: auto auto auto; gap: 10px; text-align: center; padding: 10px;",

            # Cell 1: Wind
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
              tags$h5("Wind"),
              wind_content
            ),

            # Cell 2: Temperature
            tags$div(
              style = "background-color: #f0f0f0; padding: 0px; border-radius: 8px;",
              tags$h5("Temperature"),
              p(strong("Air Temperature (°C):"), latest_data$airtemperature),
              plotlyOutput(ns("temperatureGauge"), height = "250px", width = "200px"),
              p("Range calculated based on the temperature from last 24hrs.")
            ),

            # Cell 3: Humidity
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
              tags$h5("Humidity"),
              p(
                strong("Humidity (%):"),
                ifelse(
                  is.na(latest_data$relativehumidity) | is.null(latest_data$relativehumidity),
                  "N/A",
                  format(round(latest_data$relativehumidity, 2), nsmall = 2)
                )
              )
            ),

            # Cell 4: Pressure
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
              tags$h5("Pressure"),
              p(
                strong("Pressure (hPa):"),
                ifelse(
                  is.na(latest_data$atmosphericpressure) | is.null(latest_data$atmosphericpressure),
                  "N/A",
                  format(round(latest_data$atmosphericpressure, 2), nsmall = 2)
                )
              ),
            ),

            # Cell 5: Air Quality
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
              tags$h5("Air Quality"),
              p(
                strong("PM2.5 (µg/m³):"),
                ifelse(
                  is.na(latest_data$pm25) | is.null(latest_data$pm25),
                  "N/A",
                  format(round(latest_data$pm25, 2), nsmall = 2)
                )
              ),
            ),

            # Cell 6: Noise
            tags$div(
              style = "background-color: #f0f0f0; padding: 20px; border-radius: 8px;",
              tags$h5("Noise"),
              p(
                strong("Noise (dB):"),
                ifelse(
                  is.na(latest_data$noise) | is.null(latest_data$noise),
                  "N/A",
                  format(round(latest_data$noise, 2), nsmall = 2)
                )
              )
            )
          ),
        )

        # Show the modal with the latest data
        showModal(modalDialog(
          title = "Latest Sensor Data",
          modal_content,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))

        # Plot compass with the wind direction
        runjs(sprintf("renderCompass(%s, %s);", direction, wind_speed))
      } else {
        # Show an error message if the request fails
        showModal(modalDialog(
          title = "Error",
          "Failed to retrieve data. Please try again later.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })

    # Select sensor to filter time series plot
    observeEvent(input$microclimateMap_marker_click, {
      click <- input$microclimateMap_marker_click
      sensor_id <- click$id # Assumes that each marker has a unique ID corresponding to a sensor

      # Update the selected sensor value
      selected_sensor(sensor_id)

      data_ <- df %>% filter(abs(as.numeric(difftime(received_at, input$timeSlider, units = "secs"))) < 450)

      time <- data_ %>%
        filter(device_id == sensor_id) %>%
        pull(received_at) %>%
        unique()

      # Debug message
      print(sensor_id)
      print(input$timeSlider)
      print(time)
      print(data_)

      if (!autoPlay() && !is.null(selected_sensor())) {
        formatted_time <- format(time[1], "%d.%m.%Y %H:%M:%S")

        print(formatted_time)
        print(class(formatted_time))

        # Trigger JavaScript to filter Tableau visualization
        runjs(sprintf(
          'let viz = document.getElementById("tableauViz");
        let sheet = viz.workbook.activeSheet;
        sheet.applyFilterAsync("Device Id", ["%s"], FilterUpdateType.Replace);
        ',
          sensor_id
        ))
      }
    })

    # Animation logic
    autoPlay <- reactiveVal(FALSE)
    animationTimer <- reactiveTimer(1000)

    observeEvent(input$playButton, {
      # Toggle the autoPlay state when the play button is clicked
      autoPlay(!autoPlay())

      if (autoPlay()) {
        updateActionButton(session, "playButton", label = "Pause")
      } else {
        updateActionButton(session, "playButton", label = "Play")
      }
    })

    observe({
      if (autoPlay()) {
        animationTimer() # Trigger the reactive timer when autoPlay is TRUE

        # Move to the next time step in the slider
        new_time <- input$timeSlider + 900 # Adjust step size as needed
        if (new_time > max(df$received_at)) {
          new_time <- min(df$received_at) # Loop back to the beginning if end is reached
        }

        updateSliderInput(session, "timeSlider", value = new_time)
      }
    })

    # Update map based on selected measure
    observe({
      req(input$timeSlider)
      print(class(df$received_at))
      print(class(input$timeSlider))
      df <- df %>% filter(!is.na(received_at))
      data_ <- df %>% filter(abs(as.numeric(difftime(received_at, input$timeSlider, units = "secs"))) < 450)
      print(data_)

      map <- leafletProxy("microclimateMap", data = df) %>% clearGroup("temp")

      if (input$measure == "Wind") {
        #--------------------------Wind speed and direction---------------------------#

        # Add cones to the map
        for (i in 1:nrow(data_)) {
          cone_coords <- calculate_cone(
            data_$lng[i], data_$lat[i],
            data_$averagewinddirection[i],
            data_$averagewindspeed[i] * 100
          ) # Scale cone length

          map <- map %>%
            removeControl("temperatureLegend") %>%
            addPolygons(
              lng = cone_coords[[1]],
              lat = cone_coords[[2]],
              color = "white", weight = 1,
              fillColor = "white", fillOpacity = 0.8,
              group = "temp"
            )
        }
        #--------------------------End of Wind speed and direction---------------------------#
      } else if (input$measure == "Air temperature") {
        #--------------------------Temperature---------------------------#

        # Layering 3 circles to create a diffusing effect
        map <- map %>%
          addCircleMarkers(
            data = data_, lng = ~lng, lat = ~lat,
            radius = 10,
            color = ~ temperature_palette(airtemperature),
            fillColor = ~ temperature_palette(airtemperature),
            fillOpacity = 0.6,
            stroke = FALSE,
            group = "temp"
          ) %>%
          addCircleMarkers(
            data = data_, lng = ~lng, lat = ~lat,
            radius = 15,
            color = ~ temperature_palette(airtemperature),
            fillColor = ~ temperature_palette(airtemperature),
            fillOpacity = 0.4,
            stroke = FALSE,
            group = "temp"
          ) %>%
          addCircleMarkers(
            data = data_, lng = ~lng, lat = ~lat,
            radius = 20,
            color = ~ temperature_palette(airtemperature),
            fillColor = ~ temperature_palette(airtemperature),
            fillOpacity = 0.2,
            stroke = FALSE,
            group = "temp"
          )

        # Add a legend for temperature
        map <- map %>% addLegend(
          position = "topright",
          pal = temperature_palette,
          values = c(-10, 30), # Adjust to the full range of temperatures
          title = "Temperature (°C)",
          opacity = 0.7,
          # Use a layerId to remove it later
          layerId = "temperatureLegend"
        )
        #--------------------------End of Temperature---------------------------#
      } else if (input$measure == "Humidity") {
        if (nrow(data_) > 0) {
          for (i in 1:nrow(data_)) {
            if (!is.na(data_$relativehumidity[i])) {
              num_droplets <- floor(data_$relativehumidity[i] / 10)
              print(num_droplets)
              # Loop to add droplets for each point
              for (j in 1:num_droplets) {
                # Slightly adjust the position of each droplet to avoid complete overlap
                offset_lng <- data_$lng[i] + runif(1, -0.0005, 0.0005)
                offset_lat <- data_$lat[i] + runif(1, -0.0005, 0.0005)

                map <- map %>% addCircleMarkers(
                  lng = offset_lng, lat = offset_lat,
                  radius = 10, # Adjust as needed for size
                  fillColor = "lightblue", # Adjust to match your preferred color
                  fillOpacity = 0.5, # Set the alpha (transparency)
                  stroke = FALSE, # Remove border for a cleaner look
                  group = "temp"
                )
              }
            }
          }
        }
      }
    })

    # Dynamic rendering of the Tableau visualization
    tableau_urls <- list(
      "Wind" = "https://public.tableau.com/views/micro_climate/Sheet3",
      "Air temperature" = "https://public.tableau.com/views/micro_climate/Sheet4",
      "Humidity" = "https://public.tableau.com/views/micro_climate/Sheet5",
      "Pressure" = "https://public.tableau.com/views/micro_climate/Sheet6",
      "Air quality" = "https://public.tableau.com/views/micro_climate/Sheet7",
      "Noise" = "https://public.tableau.com/views/micro_climate/Sheet8"
    )

    output$tableauVizContainer <- renderUI({
      selected_measure <- input$measure
      selected_url <- tableau_urls[[selected_measure]]

      # Render the appropriate Tableau visualization
      tableauPublicViz(
        id = "tableauViz",
        url = selected_url,
        height = "25vh"
      )
    })
  })
}
