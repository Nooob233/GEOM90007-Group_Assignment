library(shiny)

# Load the GEOM90007 Tableau in Shiny library
source("tableau-in-shiny-v1.2.R")

# Source Components
source("src/header.R")
source("src/attractions.R")
source("src/transit.R")
source("src/stays.R")
source("src/climate.R")

# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "globals.css")
  ),
  uiOutput("theme_css"),

  # Use header from header.R
  header_ui("header"),

  # Main Content
  uiOutput("main_content")
)

# Define server logic
server <- function(input, output, session) {
  # Call header server logic from header.R
  header_return <- header_server("header")

  # Get current_tab and theme from header module
  current_tab <- header_return$current_tab
  theme <- header_return$theme

  # Load theme CSS based on user selection
  output$theme_css <- renderUI({
    theme_value <- theme()
    theme_file <- switch(theme_value,
      "Zinc" = "theme/theme-zinc.css",
      "Violet" = "theme/theme-violet.css",
      "theme/theme-zinc.css"
    )
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = theme_file))
  })

  # Main content logic
  output$main_content <- renderUI({
    switch(current_tab(),
      "Attractions" = attractions_ui("attractions"),
      "Transit" = transit_ui("transit"),
      "Stays" = stays_ui("stays"),
      "Climate" = climate_ui("climate")
    )
  })

  # Call the respective server logic based on the active tab
  observe({
    if (current_tab() == "Stays") {
      stays_server("stays", theme = theme)
    } else if (current_tab() == "Attractions") {
      attractions_server("attractions")
    } else if (current_tab() == "Transit") {
      transit_server("transit")
    } else if (current_tab() == "Climate") {
      climate_server("climate")
    }
  })
}

# Run the application
shinyApp(ui, server, options = list(launch.browser = TRUE))
