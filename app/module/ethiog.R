library(shiny)
library(dplyr)
library(plotly)
library(rjson)
library(jsonlite)
library(here)

# Debug: Check if the GeoJSON file exists
geojson_path <- here::here("UI/www", "ethiopia_regions_map_simple.json")
if (!file.exists(geojson_path)) {
    stop("Error: GeoJSON file not found at ", geojson_path)
}

# Load GeoJSON
eth_geojson <- rjson::fromJSON(file = geojson_path)

# Check if the dataset exists
rds_path <- here::here("fetched_data/main.rds")
if (!file.exists(rds_path)) {
    warning("Warning: main.rds file not found! Using empty dataset.")
    region_data <- data.frame(subgroup = character(), estimate = numeric())
} else {
    main_data <- readRDS(rds_path)

    if (!"subgroup" %in% names(main_data)) {
        stop("Error: The 'subgroup' column does not exist in main_data.")
    }

    region_data <- main_data %>%
        filter(dimension == "Region") %>%
        group_by(subgroup) %>%
        summarize(estimate = mean(estimate, na.rm = TRUE)) %>%
        filter(!is.na(subgroup)) %>%
        mutate(
            subgroup = trimws(subgroup),
            subgroup = gsub("\\s+", " ", subgroup)
        )
}

# Set Mapbox Access Token
#mapboxToken <- "YOUR_MAPBOX_ACCESS_TOKEN"
mapboxToken <- "pk.eyJ1IjoiaG1vcmdhbnN0ZXdhcnQiLCJhIjoiY2tmaTg5NDljMDBwbDMwcDd2OHV6cnd5dCJ9.8eLR4FtlO079Gq0NeSNoeg"

# Define UI
ui <- fluidPage(
    titlePanel("Ethiopia Regional Map Visualization"),
    sidebarLayout(
        sidebarPanel(
            h4("Filters"),
            selectInput("region", "Select Region:", choices = unique(region_data$subgroup), selected = NULL, multiple = TRUE),
            actionButton("refresh", "Update Map", class = "btn btn-primary")
        ),
        mainPanel(
            plotlyOutput("cPlot", height = "90vh")
        )
    )
)

# Define Server
server <- function(input, output, session) {
    filtered_data <- reactive({
        if (is.null(input$region) || length(input$region) == 0) {
            return(region_data)
        } else {
            return(region_data %>% filter(subgroup %in% input$region))
        }
    })

    observe({
        req(file.exists(rds_path))
        main_data <- readRDS(rds_path)

        region_data <- main_data %>%
            filter(dimension == "Region") %>%
            group_by(subgroup) %>%
            summarize(estimate = mean(estimate, na.rm = TRUE))

        updateSelectInput(session, "region", choices = unique(region_data$subgroup))
    })

    output$cPlot <- renderPlotly({
        req(nrow(filtered_data()) > 0) # Ensure data is available
        plot_ly(
            type = "choroplethmapbox",
            geojson = eth_geojson,
            locations = filtered_data()$subgroup,
            z = filtered_data()$estimate,
            colorscale = "Viridis",
            featureidkey = "properties.shapeName",
            marker = list(opacity = 0.7)
        ) %>%
            layout(
                mapbox = list(
                    style = "light",
                    zoom = 4.5,
                    center = list(lon = 39.6, lat = 8.6),
                    accesstoken = mapboxToken
                ),
                margin = list(t = 20, b = 20, l = 20, r = 20)
            )
    })
}

# Run the Shiny App
shinyApp(ui, server)
