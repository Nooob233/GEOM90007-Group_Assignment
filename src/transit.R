library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(tidyr)

# Load data files
tram_stops <- read.csv("data/tram_stops.csv")
pedestrian_data <- read.csv("data/pedestrian_hourday_avg.csv")

# Separate latitude and longitude information into individual columns in pedestrian data
pedestrian_data <- pedestrian_data %>%
  separate(location, into = c("Latitude", "Longitude"), sep = ", ") %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Organize route numbers, separating purely numeric and alphanumeric routes
all_routes <- unique(unlist(strsplit(tram_stops$ROUTEUSSP, ",")))
numeric_routes <- sort(as.numeric(all_routes[grepl("^[0-9]+$", all_routes)]), na.last = TRUE)
alphanumeric_routes <- sort(all_routes[!grepl("^[0-9]+$", all_routes)])
sorted_routes <- c("None", 1, "3/3a", numeric_routes[!numeric_routes %in% c(1)], alphanumeric_routes)

# Define the boundaries of the Free Tram Zone
free_tram_zone <- matrix(c(
  144.938754, -37.814345,
  144.942039, -37.820801,
  144.950913, -37.822001,
  144.974612, -37.815319,
  144.970972, -37.807614,
  144.944901, -37.814705,
  144.941509, -37.813312
), ncol = 2, byrow = TRUE)

# Custom icons for free tram zone, airports, and train stations
free_tram_icon <- makeIcon(iconUrl = "public/free.png", iconWidth = 40, iconHeight = 40) # Slightly increased icon size
airport_icon <- makeIcon(iconUrl = "public/airport.png", iconWidth = 30, iconHeight = 30)
train_station_icon <- makeIcon(iconUrl = "public/train_station.png", iconWidth = 30, iconHeight = 30)

# Define airport data
public_airports <- data.frame(
  Name = c("Avalon Airport", "Essendon Airport", "Moorabbin Airport", "Melbourne Airport"),
  Latitude = c(-38.0394, -37.7281, -37.9757, -37.6733),
  Longitude = c(144.4711, 144.9003, 145.1000, 144.8433)
)

# Define train station data
train_stations <- data.frame(
  Name = c(
    "Flinders Street Station", "Southern Cross Station", "Richmond Station",
    "Parkville Station (Future)", "Melbourne Central Station"
  ),
  Latitude = c(-37.8181, -37.8183, -37.8236, -37.7999, -37.8100),
  Longitude = c(144.9670, 144.9525, 144.9889, 144.9592, 144.9628)
)

# Define UI
transit_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/transit.css")
    ),
    div(
      style = "position: relative; height: 600px;", # Set map to fixed height
      leafletOutput(ns("map"), height = "600px"), # Adjusted height to 600px
      
      # Absolute panel in the top-right corner for tram route and pedestrian data time filtering
      absolutePanel(
        top = 20, right = 20, width = 300, class = "absolute-panel",
        style = "background-color: rgba(255, 255, 255, 0.8);",
        selectInput(ns("route"), "Select Tram Route", choices = c("All", sorted_routes), selected = "None"),
        selectInput(ns("hourday"), "Select Hour of Day for Pedestrian Density", choices = c("None", unique(pedestrian_data$hourday)), selected = "None"), # Default set to "None"
        p("Please click two locations on the map to navigate."),
        actionButton(ns("navigate"), "Navigate"),
        actionButton(ns("clear_selection"), "Clear Selection") # Clear selection button
      )
    )
  )
}

# Server logic
transit_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Filter tram stop data based on user-selected route
    filtered_data <- reactive({
      if (input$route == "All") {
        tram_stops # Return all stops
      } else if (input$route == "None") {
        tram_stops[0, ] # Return empty data
      } else {
        # Filter tram stops containing the selected route
        tram_stops %>% filter(str_detect(ROUTEUSSP, paste0("\\b", input$route, "\\b")))
      }
    })
    
    # Filter pedestrian data based on user-selected time period
    filtered_pedestrian_data <- reactive({
      if (input$hourday == "None") {
        pedestrian_data[0, ] # Return empty data
      } else {
        # Filter pedestrian data for the selected time and round the average pedestrian count
        pedestrian_data %>%
          filter(hourday == input$hourday) %>%
          mutate(avg_pedestrian_count = as.integer(round(avg_pedestrian_count)))
      }
    })
    
    # Custom cluster styling for markers
    custom_cluster_options <- markerClusterOptions(
      disableClusteringAtZoom = 17, # Disable clustering at zoom level 17
      iconCreateFunction = JS("
        function (cluster) {
          var markers = cluster.getAllChildMarkers();
          var totalPedestrians = 0;
          markers.forEach(function(marker) {
            totalPedestrians += marker.options.avgPedestrianCount;
          });
          totalPedestrians = Math.round(totalPedestrians);
          var color = totalPedestrians < 100 ? 'green' : totalPedestrians < 500 ? 'orange' : 'red';
          return new L.DivIcon({
            html: '<div style=\"background-color:' + color + '; border-radius: 50%; width: 40px; height: 40px; display: flex; align-items: center; justify-content: center;\"><span>' + totalPedestrians + '</span></div>',
            className: 'marker-cluster', iconSize: new L.Point(40, 40)
          });
        }
      ")
    )
    
    # Initialize navigation points
    navigation_points <- reactiveVal(vector("list", 2))
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(lng = free_tram_zone[, 1], lat = free_tram_zone[, 2], color = "green", fillOpacity = 0.2, weight = 2) %>%
        addMarkers(lng = 144.957914, lat = -37.815706, icon = free_tram_icon, popup = "Free Tram Zone Icon") %>%
        addCircleMarkers(
          data = filtered_data(),
          ~LONGITUDE, ~LATITUDE,
          popup = ~ paste("Stop ID:", STOP_ID, "<br>", "Stop Name:", STOP_NAME, "<br>", "Latitude:", LATITUDE, "<br>", "Longitude:", LONGITUDE, "<br>", "Ticket Zone:", TICKETZONE, "<br>", "Route:", ROUTEUSSP),
          radius = 2, color = "blue", fill = TRUE,
          fillOpacity = ifelse(input$route == "None", 0, 0.7),
          opacity = ifelse(input$route == "None", 0, 1)
        ) %>%
        addCircleMarkers(
          data = filtered_pedestrian_data(),
          lng = ~Longitude, lat = ~Latitude,
          popup = ~ paste("Sensor Name:", sensor_name, "<br>", "Average Pedestrian Count:", avg_pedestrian_count),
          clusterOptions = custom_cluster_options,
          options = markerOptions(avgPedestrianCount = ~avg_pedestrian_count)
        ) %>%
        addMarkers(
          data = public_airports,
          lng = ~Longitude, lat = ~Latitude,
          icon = airport_icon,
          popup = ~ paste("Airport Name:", Name)
        ) %>%
        addMarkers(
          data = train_stations,
          lng = ~Longitude,
          lat = ~Latitude,
          icon = train_station_icon,
          popup = ~ paste("Train Station:", Name)
        ) %>%
        setView(lng = 144.957914, lat = -37.815706, zoom = 14)
    })
    
    # Handle map clicks for navigation points
    observeEvent(input$map_click, {
      click_info <- input$map_click
      current_points <- navigation_points()
      
      # Check if there is an available slot for a navigation point
      if (sum(sapply(current_points, is.null)) > 0) {
        empty_slot <- which(sapply(current_points, is.null))[1]
        current_points[[empty_slot]] <- c(latitude = click_info$lat, longitude = click_info$lng)
        navigation_points(current_points)
        
        # Add the marker for navigation point
        leafletProxy("map") %>%
          addMarkers(lng = click_info$lng, lat = click_info$lat, popup = paste("Location:", click_info$lat, ",", click_info$lng), group = "navigation_markers")
      }
    })
    
    # Navigation functionality
    observeEvent(input$navigate, {
      selected_points <- navigation_points()
      
      if (all(!sapply(selected_points, is.null))) {
        start <- paste(selected_points[[1]]["latitude"], selected_points[[1]]["longitude"], sep = ",")
        end <- paste(selected_points[[2]]["latitude"], selected_points[[2]]["longitude"], sep = ",")
        maps_url <- sprintf("https://www.google.com/maps/dir/?api=1&origin=%s&destination=%s", start, end)
        browseURL(maps_url)
      } else {
        showModal(modalDialog(
          title = "Oops, navigation went wrong",
          "Please select two locations on the map for navigation.",
          easyClose = TRUE
        ))
      }
    })
    
    # Clear selection functionality
    observeEvent(input$clear_selection, {
      navigation_points(vector("list", 2)) # Reset navigation points
      leafletProxy("map") %>%
        clearGroup("navigation_markers") # Only clear navigation markers group
    })
  })
}

