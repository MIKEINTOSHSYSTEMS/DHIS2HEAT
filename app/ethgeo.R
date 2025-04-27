library(shiny)
library(dplyr)
library(highcharter)
library(rjson)
library(jsonlite)
library(here)

# Load GeoJSON map data
eth_geojson <- rjson::fromJSON(file = "./www/ethiopia_regions_map_simple.json")
zone_geojson <- rjson::fromJSON(file = "./www/GeoLevel3_Zone_organisationUnits.json")
woreda_geojson <- rjson::fromJSON(file = "./www/GeoLevel4_Woreda_organisationUnits.json")

# UI Module
ethgeoUI <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            class = "map-container",
            highchartOutput(ns("cPlot"), height = "800px")
        )
    )
}

# Server Module
ethgeoServer <- function(id, filtered_data) {
    moduleServer(id, function(input, output, session) {
        # Reactive expression to process the filtered data for the map
        map_data <- reactive({
            req(filtered_data())

            # Get the selected dimension from the filtered data
            selected_dimension <- unique(filtered_data()$dimension)

            # Process data based on dimension
            processed_data <- filtered_data() %>%
                group_by(subgroup) %>%
                summarise(
                    estimate = mean(estimate, na.rm = TRUE),
                    indicator = first(indicator_name),
                    dimension = first(dimension),
                    date = first(date),
                    .groups = "drop"
                ) %>%
                filter(!is.na(subgroup)) %>%
                mutate(
                    subgroup = trimws(subgroup),
                    subgroup = gsub("\\s+", " ", subgroup),
                    name = subgroup # Important for highcharter tooltip
                )

            return(list(data = processed_data, dimension = selected_dimension))
        })

        # Get the appropriate GeoJSON based on dimension
        get_geojson <- function(dimension) {
            switch(dimension,
                "Region" = eth_geojson,
                "Zone" = zone_geojson,
                "Woreda" = woreda_geojson,
                eth_geojson
            ) # Default to region if dimension not matched
        }

        # Get the appropriate join field based on dimension
        get_join_field <- function(dimension) {
            switch(dimension,
                "Region" = "shapeName",
                "Zone" = "name",
                "Woreda" = "name",
                "shapeName"
            ) # Default to region if dimension not matched
        }

        output$cPlot <- renderHighchart({
            req(nrow(map_data()$data) > 0)

            # Viridis palette
            viridis_colors <- viridisLite::viridis(10)

            # Get the appropriate GeoJSON and join field
            current_geojson <- get_geojson(map_data()$dimension)
            join_field <- get_join_field(map_data()$dimension)

            # Create the map
            hc <- highchart(type = "map") %>%
                hc_add_series_map(
                    map = current_geojson,
                    df = map_data()$data,
                    joinBy = c(join_field, "subgroup"),
                    value = "estimate",
                    name = "Estimate",
                    dataLabels = list(enabled = TRUE, format = "{point.name}")
                ) %>%
                hc_colorAxis(stops = color_stops(n = 10, colors = viridis_colors)) %>%
                hc_mapNavigation(enabled = TRUE) %>%
                hc_title(text = paste(map_data()$dimension, "Estimates")) %>%
                hc_subtitle(text = "Hover over areas to see details") %>%
                hc_chart(backgroundColor = NULL) %>%
                hc_tooltip(
                    useHTML = TRUE,
                    headerFormat = "<b>{point.indicator}</b><br>",
                    pointFormat = paste(
                        "Area: <b>{point.subgroup}</b><br>",
                        "Dimension: <b>{point.dimension}</b><br>",
                        "Estimate: <b>{point.value:.2f}</b><br>",
                        "Date: <b>{point.date}</b>"
                    )
                ) %>%
                hc_legend(title = list(text = "Estimate Value")) %>%
                hc_exporting(enabled = TRUE)

            # Adjust map projection based on dimension
            if (map_data()$dimension %in% c("Zone", "Woreda")) {
                hc <- hc %>%
                    hc_mapNavigation(enabled = TRUE) %>%
                    hc_chart(
                        mapNavigation = list(
                            enableMouseWheelZoom = TRUE,
                            enableDoubleClickZoom = TRUE
                        )
                    )
            }

            return(hc)
        })
    })
}