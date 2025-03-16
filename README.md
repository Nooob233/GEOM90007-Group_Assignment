# Melbourne Tours - R Shiny Application

Welcome to the **Melbourne Tours** R Shiny application! This interactive tool is designed for tourists visiting Melbourne City, providing valuable insights into attractions, transit options, accommodations, and climate data to help you plan your trip effectively.

> Run the application here [`app.R`](app.R)

## Table of Contents

- [Features](#features)
- [Attractions](#attractions)
- [Transit](#transit)
- [Stays](#stays)
- [Climate](#climate)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Dependencies](#dependencies)
- [Data Sources](#data-sources)

## Features

### Attractions

- **Pedestrian Heatmap**: Visualise pedestrian traffic around Melbourne's landmarks over time.
- **Interactive Map**: Explore attractions using an integrated Tableau Public visualisation.

### Transit

- **Tram Routes**: View tram stops and routes, including the Free Tram Zone.
- **Pedestrian Density**: Analyse pedestrian density at different times of the day.
- **Navigation**: Select two points on the map to get navigation directions via Google Maps.
- **Key Locations**: Locate airports and train stations on the map.

### Stays

- **Accommodations Map**: Find hotels within Melbourne, displayed on an interactive map.
- **Hotel Details**: View detailed information about hotels, including prices, ratings, and descriptions.
- **Booking Links**: Access direct links to booking websites.
- **Directions**: Get directions from your current location to the hotel.

### Climate

- **Microclimate Data**: Explore real-time data on wind, temperature, humidity, pressure, air quality, and noise levels.
- **Visualisation**: Interactive maps and gauges to visualise climate conditions.
- **Time Slider**: Analyse climate data over time with an adjustable slider.
- **Sensor Selection**: Click on sensors to view detailed data and graphs.

## Installation

### Prerequisites

- **R** (version 4.0 or higher)
- **RStudio** (recommended)
- **Git** (optional, for cloning the repository)

### Steps

1. **Clone the Repository**

```bash
git clone https://github.com/JiaruiNi/GEOM90007-Assignments.git
```

2. **Navigate to the Project Directory**

```bash
cd MelbourneTours
```

3. **Install Required Packages**

Open the R console or RStudio and run:

```R
install.packages(c(
"shiny", "leaflet", "dplyr", "ggplot2", "ggiraph", "sf", "readr",
"tidyverse", "tidyr", "ggbump", "leaflet.extras", "leafpop",
"rnaturalearth", "data.tree", "readxl", "igraph", "networkD3",
"DiagrammeR", "classInt", "data.table", "shinyjs", "httr",
"jsonlite", "plotly", "shinycssloaders"
))
```

4. **Run the Application**

[`app.R`](app.R)

## Usage

1. **Launch the App**

After running `app.R`, the application should open in your default web browser.

2. **Navigate Through Tabs**

- **Attractions**: Explore pedestrian heatmaps and landmarks.
- **Transit**: View tram routes and navigate between locations.
- **Stays**: Browse hotels and accommodation options.
- **Climate**: Analyse microclimate data across Melbourne.

3. **Interactive Features**

- **Maps**: Click on map markers for more information.
- **Sliders and Selectors**: Use input controls to filter data.
- **Navigation**: Use the navigation feature in the Transit tab to get directions.

## Project Structure

```
MelbourneTours
├── app.R
├── data
│ ├── hotels.csv
│ └── ...
├── src
│ ├── attractions.R
│ ├── climate.R
│ ├── header.R
│ ├── stays.R
│ └── transit.R
├── www
│ ├── fonts
│ │ ├── Inter-Italic-VariableFont_opszwght.ttf
│ │ └── Inter-VariableFont_opszwght.ttf
│ ├── globals.css
│ ├── public
│ │ ├── arrow.png
│ │ ├── location_Zinc.png
│ │ └── ...
│ ├── scripts
│ │ ├── compass.js
│ │ └── header.js
│ ├── styles
│ │ ├── header.css
│ │ └── stays.css
│ └── theme
│ ├── theme-violet.css
│ └── theme-zinc.css
```

## Dependencies

- **R Packages**:
- shiny
- leaflet
- dplyr
- ggplot2
- ggiraph
- sf
- readr
- tidyverse
- tidyr
- ggbump
- leaflet.extras
- leafpop
- rnaturalearth
- data.tree
- readxl
- igraph
- networkD3
- DiagrammeR
- classInt
- data.table
- shinyjs
- httr
- jsonlite
- plotly
- shinycssloaders

- **JavaScript Libraries**:
- D3.js (for compass visualisation)
- Tableau JavaScript API (for embedding Tableau visualisations)

## Data Sources

- **City of Melbourne Open Data Portal**:
- Microclimate sensor data
- Pedestrian counting system
- Tram stop information

- **External APIs**:
- Google Maps API (for navigation)
