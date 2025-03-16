library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(jsonlite)

# Read the hotels.csv data
hotels_data <- read_csv("data/hotels.csv")

# Select necessary columns
hotels <- hotels_data %>%
  select(address, cover_alt, cover_src, description, latitude, link, longitude, price, price_currency, price_for, price_specification, recommended_unit, recommended_bed, score, score_aggregate, score_reviews, suburb, title)

# Stays UI
stays_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/stays.css"),
      tags$script(HTML("
    function getDirections(hotelAddress) {
      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(function(position) {
          var userLat = position.coords.latitude;
          var userLng = position.coords.longitude;
          var origin = userLat + ',' + userLng;
          var destination = encodeURIComponent(hotelAddress);
          var mapsUrl = 'https://www.google.com/maps/dir/?api=1&origin=' + origin + '&destination=' + destination + '&travelmode=driving';
          window.open(mapsUrl, '_blank');
        }, function(error) {
          alert('Unable to retrieve your location.');
        });
      } else {
        alert('Geolocation is not supported by this browser.');
      }
    }
  "))
    ),
    fluidRow(
      column(
        8,
        div(
          class = "hotels-map-container",
          leafletOutput(ns("hotels_map"), height = "800px")
        )
      ),
      column(
        4,
        div(
          style = "height: 800px; overflow-y: auto;",
          uiOutput(ns("hotel_panel"))
        )
      )
    )
  )
}

# Stays Server
stays_server <- function(id, theme) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store selected hotel ID
    selected_hotel_id <- reactiveVal(NULL)

    # Render the left panel: hotel list or hotel details
    output$hotel_panel <- renderUI({
      ns <- session$ns
      if (is.null(selected_hotel_id())) {
        # Render hotel list
        tagList(
          p(nrow(hotels), "places within City of Melbourne"),
          uiOutput(ns("hotel_list"))
        )
      } else {
        # Determine icon URL based on theme
        current_theme <- theme()
        icon_url <- paste0("public/location_", current_theme, ".png")
        # Render selected hotel details with reset button
        selected_hotel <- hotels %>% filter(title == selected_hotel_id())
        hotel <- selected_hotel[1, ]
        address_js <- jsonlite::toJSON(hotel$address, auto_unbox = TRUE)
        # Convert description to paragraphs
        description_paragraphs <- paste0("<p class='hotel-description-paragraph'>", gsub("\n", "</p><p class='hotel-description-paragraph'>", hotel$description), "</p>")
        fluidRow(
          column(
            width = 12,
            div(
              class = "hotel-details-container",
              div(
                class = "hotel-details-header",
                div(
                  class = "hotel-details-title-adress-container",
                  h3(
                    class = "hotel-details-title",
                    hotel$title
                  ),
                  div(
                    class = "hotel-details-address-container",
                    onclick = paste0("getDirections(", address_js, ")"),
                    img(
                      class = "hotel-details-address-icon",
                      src = icon_url,
                      alt = "Location Icon",
                      draggable = "false"
                    ),
                    p(
                      class = "hotel-details-address",
                      hotel$address
                    )
                  )
                ),
                div(
                  class = "hotel-details-reset-container",
                  onclick = paste0("Shiny.setInputValue('", ns("reset_selection"), "', '', {priority: 'event'})"),
                  div(
                    class = "hotel-details-reset-title",
                    "Reset",
                    img(class = "hotel-details-reset-icon", src = "public/close.svg", alt = "Reset Icon", draggable = "false")
                  )
                )
              ),
              div(
                class = "hotel-details-body",
                div(
                  class = "hotel-description-container",
                  HTML(description_paragraphs)
                ),
                div(
                  class = "hotel-description-booking-link-container",
                  a(
                    href = hotel$link, target = "_blank", style = "text-decoration: none; color: inherit;",
                    onclick = "event.stopPropagation();",
                    div(
                      class = "hotel-description-booking-link-title",
                      "Reserve"
                    )
                  )
                ),
                div(
                  class = "hotel-description-direction-container",
                  onclick = paste0("getDirections(", address_js, ")"),
                  div(
                    class = "hotel-description-direction-title",
                    "Get Directions",
                  )
                )
              )
            )
          )
        )
      }
    })

    # Observe hotel selection events
    observeEvent(input$hotel_selected, {
      selected_hotel_id(input$hotel_selected)
      # Clear any existing popups on the map
      leafletProxy(session$ns("hotels_map")) %>% clearPopups()
    })

    # Render the hotel list as a two-column layout using fluidRow and column
    output$hotel_list <- renderUI({
      ns <- session$ns

      # Split hotel items into a list of fluidRow elements with two columns each
      hotel_rows <- lapply(seq(1, nrow(hotels), by = 2), function(i) {
        hotel1 <- hotels[i, ]
        hotel2 <- if (i + 1 <= nrow(hotels)) hotels[i + 1, ] else NULL

        fluidRow(
          style = "margin: 1; padding: 1;",
          column(
            width = 6,
            style = "margin: 1; padding: 1;",
            if (!is.null(hotel1)) {
              # Render the first hotel in the row
              div(
                class = "hotel-list-wrapper",
                div(
                  class = "hotel-list-booking-link-container",
                  a(
                    href = hotel1$link, target = "_blank", style = "text-decoration: none; color: inherit;",
                    onclick = "event.stopPropagation();",
                    div(
                      class = "hotel-list-booking-link-title",
                      "Book",
                      img(class = "hotel-list-booking-link-icon", src = "public/arrow.png", alt = "Book Icon", draggable = "false")
                    )
                  )
                ),
                div(
                  class = "hotel-list-container",
                  onclick = paste0("Shiny.setInputValue('", ns("hotel_selected"), "', '", hotel1$title, "', {priority: 'event'})"),
                  img(
                    class = "hotel-list-image", src = hotel1$cover_src, alt = hotel1$cover_alt, draggable = "false",
                    onerror = "this.onerror=null;this.src='www/public/OnlineError.jpg';"
                  ),
                  div(
                    class = "hotel-list-content",
                    div(
                      class = "hotel-list-title-container",
                      h4(class = "hotel-list-title", hotel1$title),
                      div(
                        class = "hotel-list-rating-container",
                        img(class = "hotel-list-star-icon", src = "public/star.svg", alt = "Star Icon", draggable = "false"),
                        p(class = "hotel-list-score", hotel1$score)
                      )
                    ),
                    div(
                      class = "hotel-list-recommended-container",
                      p(class = "hotel-list-recommended-unit", hotel1$recommended_unit),
                      p(class = "hotel-list-recommended-bed", hotel1$recommended_bed)
                    ),
                    div(
                      class = "hotel-list-price-container",
                      p(class = "hotel-list-price", paste0("$", hotel1$price)),
                      p(class = "hotel-list-price-currency", hotel1$price_currency),
                      p(class = "hotel-list-price-for", hotel1$price_for)
                    )
                  )
                )
              )
            }
          ),
          column(
            width = 6,
            style = "margin: 1; padding: 1;",
            if (!is.null(hotel2)) {
              # Render the first hotel in the row
              div(
                class = "hotel-list-wrapper",
                div(
                  class = "hotel-list-booking-link-container",
                  a(
                    href = hotel2$link, target = "_blank", style = "text-decoration: none; color: inherit;",
                    onclick = "event.stopPropagation();",
                    div(
                      class = "hotel-list-booking-link-title",
                      "Book",
                      img(class = "hotel-list-booking-link-icon", src = "public/arrow.png", alt = "Book Icon", draggable = "false")
                    )
                  )
                ),
                div(
                  class = "hotel-list-container",
                  onclick = paste0("Shiny.setInputValue('", ns("hotel_selected"), "', '", hotel2$title, "', {priority: 'event'})"),
                  img(
                    class = "hotel-list-image", src = hotel2$cover_src, alt = hotel2$cover_alt, draggable = "false",
                    onerror = "this.onerror=null;this.src='www/public/OnlineError.jpg';"
                  ),
                  div(
                    class = "hotel-list-content",
                    div(
                      class = "hotel-list-title-container",
                      h4(class = "hotel-list-title", hotel2$title),
                      div(
                        class = "hotel-list-rating-container",
                        img(class = "hotel-list-star-icon", src = "public/star.svg", alt = "Star Icon", draggable = "false"),
                        p(class = "hotel-list-score", hotel2$score)
                      )
                    ),
                    div(
                      class = "hotel-list-recommended-container",
                      p(class = "hotel-list-recommended-unit", hotel2$recommended_unit),
                      p(class = "hotel-list-recommended-bed", hotel2$recommended_bed)
                    ),
                    div(
                      class = "hotel-list-price-container",
                      p(class = "hotel-list-price", paste0("$", hotel2$price)),
                      p(class = "hotel-list-price-currency", hotel2$price_currency),
                      p(class = "hotel-list-price-for", hotel2$price_for)
                    )
                  )
                )
              )
            }
          )
        )
      })

      # Use tagList to properly render all rows of hotels
      do.call(tagList, hotel_rows)
    })

    # Observe marker click events to update selected hotel
    observeEvent(input$hotels_map_marker_click, {
      event <- input$hotels_map_marker_click
      selected_hotel_id(event$id)

      # Use leafletProxy to manage popups
      leafletProxy(session$ns("hotels_map")) %>%
        clearPopups() %>%
        showPopup(event$lng, event$lat, event$id)
    })

    # Observe reset button to clear selection and close popups
    observeEvent(input$reset_selection, {
      selected_hotel_id(NULL)
      # Use leafletProxy to clear all popups
      leafletProxy(session$ns("hotels_map")) %>%
        clearPopups()
    })

    # Observe map click events (clicking on non-marker areas)
    observeEvent(input$hotels_map_click, {
      # Reset the selected hotel ID when clicking on map (not markers)
      selected_hotel_id(NULL)
      # Clear any existing popups
      leafletProxy(session$ns("hotels_map")) %>%
        clearPopups()
    })

    # Generate a unique input ID for the custom close button
    close_button_id <- session$ns("popup_custom_closed")
    observeEvent(input$popup_custom_closed, {
      # Reset the selected hotel ID
      selected_hotel_id(NULL)
      # Use leafletProxy to clear all popups
      leafletProxy(session$ns("hotels_map")) %>%
        clearPopups()
    })

    # Function to show popup content
    showPopup <- function(map, lng, lat, id) {
      hotel <- hotels %>% filter(title == id)
      if (nrow(hotel) > 0) {
        popup_content <- paste0(
          "<div class='popup-wrapper'>",
          "<div class='popup-custom-closed'>",
          "<img class='popup-custom-closed-icon' src='public/close.svg' alt='Close Icon' draggable = 'false' onclick=\"Shiny.setInputValue('", close_button_id, "', '", id, "', {priority: 'event'})\"/>",
          "</div>",
          "<div class='popup-container'>",
          "<a href='", hotel$link, "' target='_blank' style='text-decoration: none; color: inherit;'>",
          "<img class='popup-image' src='", hotel$cover_src, "' alt='", hotel$cover_alt, "' draggable = 'false' onerror=\"this.onerror=null;this.src='www/public/OnlineError.jpg';\"/>",
          "<div class='popup-content'>",
          "<div class='popup-title-container'>",
          "<h4 class='popup-title'>", hotel$title, "</h4>",
          "<div class='popup-rating-container'>",
          "<img class='popup-star-icon' src='public/star.svg' alt='Star Icon' draggable = 'false'/>",
          "<p class='popup-score'>", hotel$score, "</p>",
          "<p class='popup-reviews-num'>(", hotel$score_reviews, ")</p>",
          "</div>",
          "</div>",
          "<div class='popup-recommended-container'>",
          "<p class='popup-recommended-unit'>", hotel$recommended_unit, "</p>",
          "<p class='popup-recommended-bed'>", hotel$recommended_bed, "</p>",
          "</div>",
          "<div class='popup-price-container'>",
          "<p class='popup-price'>$", hotel$price, "</p>",
          "<p class='popup-price-currency'>", hotel$price_currency, "</p>",
          "<p class='popup-price-tag'>total</p>",
          "</div>",
          "</div>",
          "</a>",
          "</div>",
          "</div>"
        )
        map %>% addPopups(
          lng, lat, popup_content,
          layerId = id,
          options = popupOptions(closeButton = FALSE)
        )
      }
      map
    }

    # Render the hotels map
    output$hotels_map <- renderLeaflet({
      # Get the selected theme from passed argument
      current_theme <- theme()

      # Determine icon URL based on theme
      icon_url <- paste0("public/location_", current_theme, ".png")

      leaflet(data = hotels) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = mean(hotels$longitude), lat = mean(hotels$latitude), zoom = 15) %>%
        addMarkers(
          ~longitude, ~latitude,
          label = ~ paste0("$", price, " AUD"),
          labelOptions = labelOptions(
            style = list(
              "color" = "#18181B",
              "background-color" = "white",
              "border-color" = "#E4E4E7",
              "border-radius" = "200px",
              "padding" = "5px",
              "box-shadow" = "0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)"
            ),
            textsize = "14px",
            direction = "auto"
          ),
          clusterOptions = markerClusterOptions(disableClusteringAtZoom = 17),
          layerId = ~title,
          icon = icons(
            iconUrl = icon_url,
            iconWidth = 32, iconHeight = 32
          )
        )
    })
  })
}
