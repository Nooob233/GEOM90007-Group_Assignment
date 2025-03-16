library(shiny)

# Header UI
header_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/header.css"),
      tags$script(src = "scripts/header.js")
    ),
    div(
      class = "top-nav",
      # Title
      div(class = "title", "Melbourne Tours"),
      # Tabs
      div(
        class = "tabs-container",
        uiOutput(ns("tabs_ui"))
      ),
      # Theme Dropdown Selector
      div(
        class = "theme-selector wrapper",
        tags$label("Theme"),
        tags$ul(
          tags$li("Zinc", onclick = paste0("Shiny.setInputValue('", ns("theme_select"), "', 'Zinc');")),
          tags$li("Violet", onclick = paste0("Shiny.setInputValue('", ns("theme_select"), "', 'Violet');"))
        )
      )
    )
  )
}

# Header Server
header_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Namespace
    ns <- session$ns

    # Reactive value for current tab
    current_tab <- reactiveVal("Attractions")

    # Update current tab based on user interaction
    observeEvent(input$tab_attractions, {
      current_tab("Attractions")
    })

    observeEvent(input$tab_transit, {
      current_tab("Transit")
    })

    observeEvent(input$tab_stays, {
      current_tab("Stays")
    })

    observeEvent(input$tab_climate, {
      current_tab("Climate")
    })

    output$tabs_ui <- renderUI({
      tabs <- c("Attractions", "Transit", "Stays", "Climate")
      tab_ui_list <- lapply(seq_along(tabs), function(i) {
        tab_name <- tabs[i]
        tab_id <- paste0("tab_", tolower(tab_name))
        class_name <- ifelse(current_tab() == tab_name, "tab active", "tab")

        # Tab Icon Path
        icon_path <- switch(tab_name,
          "Attractions" = "public/attractions.svg",
          "Transit" = "public/transit.png",
          "Stays" = "public/stays.svg",
          "Climate" = "public/climate.png"
        )

        # Combine icon and tab name using tagList
        tab_element <- actionLink(
          inputId = ns(tab_id),
          label = tagList(
            tags$img(src = icon_path, width = "20px", height = "20px", class = "tab-icon", draggable = "false"),
            tab_name
          ),
          class = class_name
        )

        # If not the last tab, add a separator
        if (i < length(tabs)) {
          tagList(
            tab_element,
            tags$div(class = "separator")
          )
        } else {
          tab_element
        }
      })

      div(class = "tabs", do.call(tagList, tab_ui_list))
    })

    # Reactive value for theme
    theme_reactive <- reactiveVal("Zinc")

    # Update the reactive theme variable
    observeEvent(input$theme_select, {
      theme <- input$theme_select
      theme_reactive(theme)
    })

    # Return the current tab and theme
    return(list(
      current_tab = current_tab,
      theme = theme_reactive
    ))
  })
}
