library(shiny)
library(dplyr)
library(highcharter)
library(rjson)
library(jsonlite)
library(here)

# Load GeoJSON map data
eth_geojson <- rjson::fromJSON(file = "./www/ethiopia_regions_map_simple.json")

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

            # Filter for Region dimension only
            region_data <- filtered_data() %>%
                filter(dimension == "Region") %>%
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

            return(region_data)
        })

        output$cPlot <- renderHighchart({
            req(nrow(map_data()) > 0)

            # Viridis palette
            viridis_colors <- viridisLite::viridis(10)

            highchart(type = "map") %>%
                hc_add_series_map(
                    map = eth_geojson,
                    df = map_data(),
                    joinBy = c("shapeName", "subgroup"),
                    value = "estimate",
                    name = "Estimate",
                    dataLabels = list(enabled = TRUE, format = "{point.name}")
                ) %>%
                hc_colorAxis(stops = color_stops(n = 10, colors = viridis_colors)) %>%
                hc_mapNavigation(enabled = TRUE) %>%
                hc_title(text = "Regional Estimates") %>%
                hc_subtitle(text = "Hover over regions to see details") %>%
                hc_chart(backgroundColor = NULL) %>%
                hc_tooltip(
                    useHTML = TRUE,
                    headerFormat = "<b>{point.indicator}</b><br>",
                    pointFormat = paste(
                        "Region: <b>{point.subgroup}</b><br>",
                        "Dimension: <b>{point.dimension}</b><br>",
                        "Estimate: <b>{point.value:.2f}</b><br>",
                        "Date: <b>{point.date}</b>"
                    )
                ) %>%
                hc_legend(title = list(text = "Estimate Value")) %>%
                hc_exporting(enabled = TRUE)
        })
    })
}