library(shiny)
library(dplyr)
library(plotly)
library(rjson)
library(jsonlite)
library(here)

# Load GeoJSON map data
eth_geojson <- rjson::fromJSON(file = "./www/ethiopia_regions_map_simple.json")

# Debug: Verify GeoJSON file existence
print(file.exists("./www/ethiopia_regions_map_simple.json"))

# Data loading function
load_data <- function() {
    if (file.exists("fetched_data/main.rds")) {
        main_data <- readRDS("fetched_data/main.rds")

        if (!"subgroup" %in% names(main_data)) {
            stop("The 'subgroup' column does not exist in main_data.")
        }

        if (!"date" %in% names(main_data)) {
            stop("The 'date' column does not exist in main_data.")
        }

        region_data <- main_data %>%
            dplyr::filter(dimension == "Region") %>%
            dplyr::group_by(subgroup, date) %>%
            dplyr::summarize(estimate = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
            dplyr::filter(!is.na(subgroup)) %>%
            dplyr::mutate(
                subgroup = trimws(subgroup),
                subgroup = gsub("\\s+", " ", subgroup)
            )

        return(region_data)
    } else {
        print("Warning: main.rds file not found!")
        return(data.frame(subgroup = character(), estimate = numeric(), date = numeric()))
    }
}

# UI Module
ethgeoUI <- function(id) {
    ns <- NS(id)
    tagList(
        div(
            class = "filter-panel",
            fluidRow(
                column(
                    4,
                    selectInput(ns("region"), "Select Region:",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        width = "100%"
                    )
                ),
                column(
                    4,
                    selectInput(ns("date"), "Select Date:",
                        choices = NULL,
                        selected = NULL,
                        multiple = TRUE,
                        width = "100%"
                    )
                )
                #,
            #    column(
            #        4,
            #        actionButton(ns("refresh"), "Update Map",
            #            class = "btn btn-primary",
            #            style = "width: 100%; height: 38px; margin-top: 25px;"
            #        )
            #    )
            )
        ),
        div(
            class = "map-container",
            plotlyOutput(ns("cPlot"))
        )
    )
}

# Server Module
ethgeoServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        region_data <- reactiveVal(load_data())

        observe({
            updateSelectInput(session, "region", choices = unique(region_data()$subgroup))
            updateSelectInput(session, "date", choices = unique(region_data()$date))
        })

        filtered_data <- reactive({
            data <- region_data()

            if (!is.null(input$region) && length(input$region) > 0) {
                data <- data %>% dplyr::filter(subgroup %in% input$region)
            }

            if (!is.null(input$date) && length(input$date) > 0) {
                data <- data %>% dplyr::filter(date %in% input$date)
            }

            return(data)
        })

        output$cPlot <- renderPlotly({
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
                        zoom = 4,
                        center = list(lon = 39.6, lat = 8.6),
                        accesstoken = "pk.eyJ1IjoiaG1vcmdhbnN0ZXdhcnQiLCJhIjoiY2tmaTg5NDljMDBwbDMwcDd2OHV6cnd5dCJ9.8eLR4FtlO079Gq0NeSNoeg"
                    ),
                    margin = list(t = 0, b = 0, l = 0, r = 0)
                )
        })

        observeEvent(input$refresh, {
            region_data(load_data())
        })
    })
}

# Main UI
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            html, body {
                height: 100%;
                margin: 0;
                padding: 0;
                overflow: hidden;
            }

            .container-fluid {
                height: 100%;
                padding: 20px;
                margin: 0;
                display: flex;
                flex-direction: column;
            }

            .filter-panel {
                width: 100%;
                padding: 15px;
                background-color: #f8f9fa;
                border-radius: 5px;
                margin-bottom: 15px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }

            .map-container {
                flex: 1;
                width: 100%;
                position: relative;
                min-height: 400px;
            }

            .plotly.html-widget {
                height: 100% !important;
                width: 100% !important;
            }

            .selectize-input {
                width: 100% !important;
                border-radius: 4px;
                min-height: 38px;
                border: 1px solid #ced4da;
            }

            .btn-primary {
                background-color: #1da1b4;
                border-color: #1da1b4;
                transition: all 0.3s ease;
            }

            .btn-primary:hover {
                background-color: #168394;
                border-color: #168394;
                transform: translateY(-1px);
            }
        "))
    ),
    titlePanel("Ethiopia Regional Map Visualization"),
    div(
        class = "main-container",
        style = "height: calc(100vh - 80px);",
        ethgeoUI("ethgeo")
    )
)

# Server
server <- function(input, output, session) {
    ethgeoServer("ethgeo")
}

# Run application
shinyApp(ui = ui, server = server)