# spatial_module.R

spatialUI <- function(id) {
    ns <- NS(id)

    fluidRow(
        tags$head(
            tags$style(HTML("
        .map-container {
          height: 1800px;
          width: 100%;
        }
      "))
        ),
        titlePanel("WHO Benchmarking Tool"),
        sidebarLayout(
            sidebarPanel(
                width = 3,
                div(
                    class = "well",
                    selectizeInput(ns("regions"), "Select WHO Regions:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(placeholder = "Select regions")
                    ),
                    selectizeInput(ns("countries"), "Select Countries:",
                        choices = NULL,
                        multiple = TRUE,
                        options = (list(maxItems = 5))
                    ),
                    selectInput(ns("indicator"), "Select Indicator:",
                        choices = NULL
                    ),
                    selectInput(ns("dimension"), "Select Dimension:",
                        choices = NULL
                    ),
                    selectizeInput(ns("subgroup_filter"), "Filter by Subgroup:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(placeholder = "All subgroups")
                    ),
                    selectizeInput(ns("source_filter"), "Filter by Source:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(placeholder = "All sources")
                    ),
                    sliderInput(ns("date_range"), "Date Range:",
                        min = 2000, max = 2025,
                        value = c(2020, 2025),
                        step = 1
                    ),
                    checkboxInput(ns("show_average"), "Show WHO Region Average", FALSE),
                    checkboxInput(ns("show_income"), "Show Income Group Average", FALSE),
                    checkboxInput(ns("show_all_region_countries"), "Show all countries in selected regions", TRUE),
                    selectInput(ns("color_palette"), "Color Palette:",
                        choices = NULL,
                        selected = "Viridis"
                    ),
                    selectInput(ns("theme"), "Map Theme:",
                        choices = NULL,
                        selected = "Default"
                    ),
                    actionButton(ns("update"), "Update Map", class = "btn-primary")
                )
            ),
            mainPanel(
                width = 9,
                tabsetPanel(
                    tabPanel(
                        "Map",
                        div(
                            class = "map-container",
                            highchartOutput(ns("map"), height = "100%")
                        )
                    ),
                    tabPanel(
                        "Comparison Table",
                        downloadButton(ns("download_data"), "Download Data", class = "btn-primary"),
                        DT::dataTableOutput(ns("comparison_table"))
                    )
                )
            )
        )
    )
}

spatialServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Load the spatial data from JSON
        spatial_json <- "./www/spatial_geo/spatial_national.json"
        spatial_data <- st_read(spatial_json, quiet = TRUE) %>%
            mutate(NAME = as.character(NAME)) # Ensure NAME is character

        # Load indicators data
        indicators_data <- read_parquet("./data/indicators_data/HEAT_who_indicators.parquet") %>%
            mutate(
                subgroup = as.character(subgroup),
                setting = as.character(setting)
            )

        # Add default favourable_indicator if missing
        if (!"favourable_indicator" %in% names(indicators_data)) {
            indicators_data$favourable_indicator <- 0
        }

        # Preprocess indicators data
        available_indicators <- indicators_data %>%
            distinct(indicator_abbr, indicator_name) %>%
            arrange(indicator_name)

        available_dimensions <- indicators_data %>%
            distinct(dimension) %>%
            filter(!is.na(dimension)) %>%
            pull(dimension)

        # Get unique regions from the data
        available_regions <- indicators_data %>%
            filter(!is.na(whoreg6)) %>%
            distinct(whoreg6) %>%
            pull(whoreg6)

        # Define Highcharts color palettes
        color_palettes <- list(
            "Viridis" = viridisLite::viridis(10),
            "Plasma" = viridisLite::plasma(10),
            "Inferno" = viridisLite::inferno(10),
            "Magma" = viridisLite::magma(10),
            "Cividis" = viridisLite::cividis(10),
            "Rocket" = viridisLite::rocket(10),
            "Mako" = viridisLite::mako(10),
            "Turbo" = viridisLite::turbo(10)
        )

        # Define Highcharts theme options
        theme_options <- list(
            "Default" = hc_theme_null(),
            "Dark Unica" = hc_theme_darkunica(),
            "Google" = hc_theme_google(),
            "Flat" = hc_theme_flat(),
            "Grid Light" = hc_theme_gridlight(),
            "Sand Signika" = hc_theme_sandsignika(),
            "Economist" = hc_theme_economist(),
            "Financial Times" = hc_theme_ft(),
            "538" = hc_theme_538(),
            "Chalk" = hc_theme_chalk(),
            "Hand Drawn" = hc_theme_handdrawn()
        )

        # Initialize inputs
        observe({
            updateSelectizeInput(session, "regions", choices = available_regions)
            updateSelectizeInput(session, "countries", choices = sort(unique(spatial_data$NAME)))
            updateSelectInput(session, "indicator",
                choices = setNames(
                    available_indicators$indicator_abbr,
                    available_indicators$indicator_name
                )
            )
            updateSelectInput(session, "dimension", choices = available_dimensions)
            updateSelectInput(session, "color_palette", choices = names(color_palettes))
            updateSelectInput(session, "theme", choices = names(theme_options))

            # Set date range based on data
            date_range <- range(indicators_data$date, na.rm = TRUE)
            updateSliderInput(session, "date_range",
                min = date_range[1],
                max = date_range[2],
                value = c(max(date_range) - 5, max(date_range))
            )
        })

        # Initial empty map
        output$map <- renderHighchart({
            hcmap(
                "custom/world-highres",
                showInLegend = FALSE,
                borderColor = "#DFFAFFFF",
                borderWidth = 0.1
            ) %>%
                hc_title(text = "Select countries/regions and click 'Update Map'") %>%
                hc_add_theme(theme_options[["Default"]]) %>%
                hc_colorAxis(
                    stops = color_stops(colors = color_palettes[["Viridis"]]),
                    type = "logarithmic"
                ) %>%
                hc_legend(enabled = TRUE)
        })

        # Update country choices based on selected regions
        observe({
            req(input$regions)

            countries_in_region <- indicators_data %>%
                filter(whoreg6 %in% input$regions) %>%
                distinct(setting) %>%
                pull(setting)

            updateSelectizeInput(session, "countries",
                choices = sort(unique(spatial_data$NAME[spatial_data$NAME %in% countries_in_region])),
                selected = input$countries
            )
        })

        # Update subgroup and source filters dynamically based on dimension AND selected countries/regions
        observe({
            req(input$indicator, input$dimension)

            # Get base data filtered by indicator and dimension
            current_data <- indicators_data %>%
                filter(
                    indicator_abbr == input$indicator,
                    dimension == input$dimension
                )

            # Further filter by selected countries if any
            if (!is.null(input$countries) && length(input$countries) > 0) {
                selected_iso3 <- spatial_data %>%
                    filter(NAME %in% input$countries) %>%
                    pull(iso3_code)

                current_data <- current_data %>%
                    filter(iso3 %in% selected_iso3)
            }

            # Further filter by selected regions if any (and no specific countries selected)
            if ((is.null(input$countries) || length(input$countries) == 0) &&
                !is.null(input$regions) && length(input$regions) > 0) {
                current_data <- current_data %>%
                    filter(whoreg6 %in% input$regions)
            }

            # Update subgroup filter choices
            updateSelectizeInput(session, "subgroup_filter",
                choices = unique(current_data$subgroup),
                selected = input$subgroup_filter
            )

            if ("source" %in% names(current_data)) {
                updateSelectizeInput(session, "source_filter",
                    choices = unique(current_data$source),
                    selected = input$source_filter
                )
            }
        })

        # When countries/regions are selected or update button is pressed
        observeEvent(input$update, {
            req(input$indicator)

            # Check if we have either countries or regions selected
            if (is.null(input$countries) && is.null(input$regions)) {
                showNotification("Please select at least one country or region", type = "warning")
                return()
            }

            ind_data <- filtered_data()
            if (is.null(ind_data) || nrow(ind_data) == 0) {
                showNotification("No data available for selected criteria", type = "warning")
                return()
            }

            indicator_info <- available_indicators %>%
                filter(indicator_abbr == input$indicator)

            higher_better <- if (nrow(indicator_info) > 0 &&
                "favourable_indicator" %in% names(indicator_info)) {
                as.logical(indicator_info$favourable_indicator[1])
            } else {
                FALSE
            }

            # Prepare hover text
            ind_data <- ind_data %>%
                mutate(
                    hover_text = paste(
                        "Country:", ifelse(is.na(setting), NAME, setting), "<br>",
                        "Value:", round(estimate, 2), "<br>",
                        if (!is.null(input$subgroup_filter) && length(input$subgroup_filter) > 0) {
                            paste("Subgroup:", subgroup, "<br>")
                        },
                        if (input$show_average && "whoreg6" %in% names(.)) paste("WHO Region:", whoreg6, "<br>"),
                        if (input$show_income && "wbincome2024" %in% names(.)) paste("Income Group:", wbincome2024, "<br>"),
                        if (!is.null(input$source_filter) && length(input$source_filter) > 0 && "source" %in% names(.)) paste("Source:", source, "<br>"),
                        "Year:", date
                    ) %>%
                        # Clean up any empty lines
                        gsub("<br>(<br>)+", "<br>", .) %>%
                        trimws()
                )

            # Create appropriate title text
            title_text <- paste0(
                indicator_info$indicator_name[1], " Comparison<br>",
                if (!is.null(input$countries) && length(input$countries) > 0) {
                    paste("Countries: ", paste(input$countries, collapse = ", "), "<br>")
                } else if (!is.null(input$regions) && length(input$regions) > 0) {
                    paste("WHO Region: ", paste(input$regions, collapse = ", "), "<br>")
                },
                "Dimension: ", input$dimension, " | ",
                "Subgroup: ", ifelse(is.null(input$subgroup_filter) || length(input$subgroup_filter) == 0,
                    "All", paste(input$subgroup_filter, collapse = ", ")
                ), " | ",
                "Source: ", ifelse(is.null(input$source_filter) || length(input$source_filter) == 0,
                    "All", paste(input$source_filter, collapse = ", ")
                )
            )

            # Calculate min and max for color scale
            zmin_val <- min(ind_data$estimate, na.rm = TRUE)
            zmax_val <- max(ind_data$estimate, na.rm = TRUE)

            # Handle case where all values are equal
            if (zmin_val == zmax_val) {
                zmin_val <- zmin_val - 0.1
                zmax_val <- zmax_val + 0.1
            }

            # Get selected color palette
            selected_palette <- color_palettes[[input$color_palette %||% "Viridis"]]

            # Create the choropleth map with Highcharts
            hc <- hcmap(
                "custom/world-highres",
                data = ind_data,
                value = "estimate",
                joinBy = c("iso-a3", "iso3"),
                name = indicator_info$indicator_name[1],
                borderColor = "#00FFFFFF",
                borderWidth = 0.1,
                nullColor = "#d3d3d3"
            ) %>%
                hc_colorAxis(
                    min = zmin_val,
                    max = zmax_val,
                    reversed = !higher_better,
                    stops = color_stops(colors = selected_palette),
                    type = "logarithmic",
                    labels = list(format = "{value}"),
                    title = list(text = indicator_info$indicator_name[1])
                ) %>%
                hc_title(text = title_text) %>%
                hc_mapNavigation(enabled = TRUE) %>%
                hc_legend(
                    enabled = TRUE,
                    align = "right",
                    verticalAlign = "bottom",
                    layout = "vertical",
                    valueDecimals = 2
                ) %>%
                hc_add_theme(theme_options[[input$theme %||% "Default"]]) %>%
                hc_chart(
                    backgroundColor = ifelse(input$theme %in% c("Dark Unica", "Chalk"),
                        "#2a2a2b", "#FFFFFF"
                    )
                ) %>%
                hc_tooltip(
                    pointFormat = "{point.hover_text}",
                    useHTML = TRUE
                )

            # Zoom to selected countries or regions
            if (!is.null(input$countries) && length(input$countries) > 0) {
                # Get the selected countries' spatial data
                selected_countries_sf <- spatial_data %>%
                    filter(NAME %in% input$countries)

                if (nrow(selected_countries_sf) > 0) {
                    # Get the bounding box of selected countries
                    bbox <- st_bbox(selected_countries_sf)

                    # Calculate center point
                    center_lon <- mean(c(bbox$xmin, bbox$xmax))
                    center_lat <- mean(c(bbox$ymin, bbox$ymax))

                    # Calculate appropriate zoom level based on bbox size
                    bbox_width <- bbox$xmax - bbox$xmin
                    bbox_height <- bbox$ymax - bbox$ymin
                    bbox_size <- max(bbox_width, bbox_height)

                    # Dynamic zoom level calculation
                    zoom_level <- case_when(
                        length(input$countries) == 1 ~ 5, # More zoom for single country
                        bbox_size > 100 ~ 2,
                        bbox_size > 50 ~ 3,
                        bbox_size > 20 ~ 4,
                        TRUE ~ 5
                    )

                    hc <- hc %>%
                        hc_mapNavigation(
                            enabled = TRUE,
                            enableMouseWheelZoom = TRUE,
                            enableDoubleClickZoom = TRUE,
                            buttonOptions = list(
                                verticalAlign = "top",
                                align = "left"
                            )
                        ) %>%
                        hc_chart(
                            events = list(
                                load = JS(sprintf("
                  function() {
                    var chart = this;
                    setTimeout(function() {
                      try {
                        chart.mapView.setCenter([%f, %f]);
                        chart.mapView.zoom = %d;
                      } catch(e) {
                        console.log('Zoom error:', e);
                      }
                    }, 500);
                  }
                ", center_lon, center_lat, zoom_level))
                            )
                        )
                }
            } else if (!is.null(input$regions) && length(input$regions) > 0) {
                # Zoom to show all countries in selected regions
                region_countries_sf <- spatial_data %>%
                    filter(iso3_code %in% intersect(ind_data$iso3, iso3_code))

                if (nrow(region_countries_sf) > 0) {
                    bbox <- st_bbox(region_countries_sf)

                    center_lon <- mean(c(bbox$xmin, bbox$xmax))
                    center_lat <- mean(c(bbox$ymin, bbox$ymin))

                    # Calculate appropriate zoom level for the region
                    bbox_width <- bbox$xmax - bbox$xmin
                    bbox_height <- bbox$ymax - bbox$ymin
                    bbox_size <- max(bbox_width, bbox_height)

                    zoom_level <- case_when(
                        bbox_size > 100 ~ 2,
                        bbox_size > 50 ~ 3,
                        bbox_size > 20 ~ 4,
                        TRUE ~ 5
                    )

                    hc <- hc %>%
                        hc_mapNavigation(
                            enabled = TRUE,
                            enableMouseWheelZoom = TRUE,
                            enableDoubleClickZoom = TRUE,
                            buttonOptions = list(
                                verticalAlign = "top",
                                align = "left"
                            )
                        ) %>%
                        hc_chart(
                            events = list(
                                load = JS(sprintf("
                  function() {
                    var chart = this;
                    setTimeout(function() {
                      try {
                        chart.mapView.setCenter([%f, %f]);
                        chart.mapView.zoom = %d;
                      } catch(e) {
                        console.log('Zoom error:', e);
                      }
                    }, 500);
                  }
                ", center_lon, center_lat, zoom_level))
                            )
                        )
                }
            }

            output$map <- renderHighchart(hc)
        })

        # Get filtered indicator data for selected countries or regions
        filtered_data <- reactive({
            req(input$indicator, input$dimension)

            # Get selected ISO3 codes - either from selected countries OR all in selected regions
            if (!is.null(input$countries) && length(input$countries) > 0) {
                selected_iso3 <- spatial_data %>%
                    filter(NAME %in% input$countries) %>%
                    pull(iso3_code)
            } else if (!is.null(input$regions) && length(input$regions) > 0) {
                # Get all countries in selected regions
                selected_iso3 <- indicators_data %>%
                    filter(whoreg6 %in% input$regions) %>%
                    distinct(iso3) %>%
                    pull(iso3)
            } else {
                return(NULL)
            }

            data <- indicators_data %>%
                filter(
                    iso3 %in% selected_iso3,
                    indicator_abbr == input$indicator,
                    dimension == input$dimension,
                    date >= input$date_range[1],
                    date <= input$date_range[2]
                )

            if (!is.null(input$subgroup_filter) && length(input$subgroup_filter) > 0) {
                data <- data %>% filter(subgroup %in% input$subgroup_filter)
            }

            if ("source" %in% names(data) && !is.null(input$source_filter) && length(input$source_filter) > 0) {
                data <- data %>% filter(source %in% input$source_filter)
            }

            if (nrow(data) == 0) {
                return(NULL)
            }

            # Calculate averages if needed
            if (input$show_average || input$show_income) {
                avg_data <- indicators_data %>%
                    filter(
                        indicator_abbr == input$indicator,
                        dimension == input$dimension,
                        date >= input$date_range[1],
                        date <= input$date_range[2]
                    )

                if (!is.null(input$subgroup_filter) && length(input$subgroup_filter) > 0) {
                    avg_data <- avg_data %>% filter(subgroup %in% input$subgroup_filter)
                }
                if ("source" %in% names(avg_data) && !is.null(input$source_filter) && length(input$source_filter) > 0) {
                    avg_data <- avg_data %>% filter(source %in% input$source_filter)
                }

                if (input$show_average && "whoreg6" %in% names(avg_data)) {
                    who_avg <- avg_data %>%
                        filter(!is.na(whoreg6)) %>%
                        group_by(whoreg6, indicator_abbr, dimension) %>%
                        summarise(
                            estimate = mean(estimate, na.rm = TRUE),
                            subgroup = paste("WHO Region:", first(whoreg6)),
                            iso3 = first(whoreg6),
                            NAME = paste("WHO Region:", first(whoreg6)),
                            .groups = "drop"
                        )
                    data <- bind_rows(data, who_avg)
                }

                if (input$show_income && "wbincome2024" %in% names(avg_data)) {
                    income_avg <- avg_data %>%
                        filter(!is.na(wbincome2024)) %>%
                        group_by(wbincome2024, indicator_abbr, dimension) %>%
                        summarise(
                            estimate = mean(estimate, na.rm = TRUE),
                            subgroup = paste("Income Group:", first(wbincome2024)),
                            iso3 = first(wbincome2024),
                            NAME = paste("Income Group:", first(wbincome2024)),
                            .groups = "drop"
                        )
                    data <- bind_rows(data, income_avg)
                }
            }

            # Get most recent data for each country
            data %>%
                group_by(iso3, subgroup) %>%
                filter(!is.na(date)) %>%
                filter(date == max(date, na.rm = TRUE)) %>%
                ungroup()
        })

        # Create comparison table
        output$comparison_table <- DT::renderDataTable({
            req(filtered_data())

            data <- filtered_data()

            cols_to_select <- c(
                "setting", "whoreg6", "wbincome2024", "indicator_name",
                "dimension", "subgroup", "estimate", "date", "source"
            )
            available_cols <- cols_to_select[cols_to_select %in% names(data)]

            comparison_data <- data %>%
                select(all_of(available_cols)) %>%
                rename(
                    Country = setting,
                    Region = whoreg6,
                    `Income Group` = wbincome2024,
                    Indicator = indicator_name,
                    Dimension = dimension,
                    Subgroup = subgroup,
                    Value = estimate,
                    Year = date,
                    Source = source
                ) %>%
                arrange(Country, Subgroup)

            DT::datatable(
                comparison_data,
                extensions = c("Buttons", "Scroller"),
                options = list(
                    scrollX = TRUE,
                    scrollY = "400px",
                    scroller = TRUE,
                    dom = "Bfrtip",
                    buttons = c("copy", "csv", "excel", "pdf"),
                    pageLength = 10,
                    lengthMenu = list(
                        c(5, 10, 20, 50, 100, -1),
                        c("5", "10", "20", "50", "100", "All")
                    )
                ),
                rownames = FALSE
            )
        })

        # Download handler for data
        output$download_data <- downloadHandler(
            filename = function() {
                paste("health_indicators_comparison_", Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                data <- filtered_data()
                if (!is.null(data)) {
                    write.csv(data, file, row.names = FALSE)
                }
            }
        )
    })
}