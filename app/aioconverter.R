# converter.R

# Set maximum upload size to 500MB (adjust if needed)
options(shiny.maxRequestSize = 500 * 1024^2) # 500MB

# Load required packages
library(shiny)
library(arrow)
library(readr) # For reading CSV
library(readxl) # For reading Excel files
library(openxlsx) # For reading problematic Excel files
library(DT) # For displaying data tables with pagination
library(tools)
library(shinyjs) # For controlling button enablement
library(RPostgres) # For PostgreSQL connectivity
library(DBI) # For database interface
library(jsonlite) # For JSON conversion
library(haven) # For handling RDA files

ui <- fluidPage(
    useShinyjs(), # Initialize shinyjs to control button enablement

    titlePanel("AIO R Data Converter (Excel/CSV/RDA to Parquet/CSV/Excel/JSON)"),
    sidebarLayout(
        sidebarPanel(
            # Tabset for different data sources
            tabsetPanel(
                id = "data_source",
                tabPanel(
                    "File Upload",
                    fileInput("file", "Upload File",
                        accept = c(".xlsx", ".xls", ".csv", ".rda", ".rds")
                    ),
                    br(),
                    radioButtons("row_selection", "Number of Rows to Convert",
                        choices = c(
                            "All" = "all",
                            "1,000" = "1000",
                            "10,000" = "10000",
                            "50,000" = "50000",
                            "500,000" = "500000"
                        ),
                        selected = "1000"
                    ),
                    conditionalPanel(
                        condition = "input.row_selection == 'custom'",
                        numericInput("custom_rows", "Custom Number of Rows", value = 1000, min = 1)
                    )
                ),
                tabPanel(
                    "PostgreSQL",
                    textInput("db_host", "Host", value = "localhost"),
                    numericInput("db_port", "Port", value = 5432),
                    textInput("db_username", "Username", value = "postgres"),
                    passwordInput("db_password", "Password", value = ""),
                    textInput("db_name", "Database", value = "HEAT"),
                    textInput("db_table", "Table", value = "who_indicators"),
                    radioButtons("db_row_selection", "Number of Rows to Convert",
                        choices = c(
                            "All" = "all",
                            "1,000" = "1000",
                            "10,000" = "10000",
                            "50,000" = "50000",
                            "500,000" = "500000"
                        ),
                        selected = "1000"
                    ),
                    conditionalPanel(
                        condition = "input.db_row_selection == 'custom'",
                        numericInput("db_custom_rows", "Custom Number of Rows", value = 1000, min = 1)
                    ),
                    actionButton("connect_db", "Connect to Database")
                )
            ),
            br(),
            actionButton("preview", "Preview Data"),
            actionButton("convert", "Convert Data", disabled = TRUE),
            br(),
            textOutput("status"),
            br(),
            selectInput("output_format", "Select Output Format",
                choices = c(
                    "Parquet" = "parquet",
                    "CSV" = "csv",
                    "Excel" = "excel",
                    "JSON" = "json"
                )
            ),
            downloadButton("download_output", "Download Converted File"),

            # Add new GeoJSON extraction button
            conditionalPanel(
                condition = "output.geojson_exists",
                hr(),
                h4("GeoJSON Tools"),
                downloadButton("download_geojson", "Download GeoJSON Data")
            )
        ),
        mainPanel(
            h4("Preview Data"),
            DTOutput("data_preview"), # Display the preview table
            h4("Conversion Log"),
            tags$style(type = "text/css", "#log {white-space: pre-wrap; overflow-y:scroll; max-height: 400px; background-color: #f8f9fa; padding: 10px;}"),
            verbatimTextOutput("log", placeholder = TRUE)
        )
    )
)

server <- function(input, output, session) {
    # Reactive values to store logs, file data, and converted file info
    rv <- reactiveValues(
        log = "",
        converted_file = NULL,
        df = NULL, # Store the data frame for preview and conversion
        db_conn = NULL, # Store database connection
        data_source = "file" # Track current data source
    )

    # Close database connection when session ends
    onStop(function() {
        if (!is.null(rv$db_conn)) {
            dbDisconnect(rv$db_conn)
            add_log("Database connection closed.")
        }
    })

    # Function to append to the log
    add_log <- function(msg) {
        timestamp <- format(Sys.time(), "%H:%M:%S")
        rv$log <- paste0(rv$log, "[", timestamp, "] ", msg, "\n")
    }

    output$log <- renderText({
        invalidateLater(1000, session)
        rv$log
    })

    # Function to get the selected number of rows
    get_num_rows <- function() {
        if (rv$data_source == "file") {
            if (input$row_selection == "all") {
                return(Inf) # Read all rows
            } else if (input$row_selection == "custom") {
                return(input$custom_rows)
            } else {
                return(as.numeric(input$row_selection))
            }
        } else {
            if (input$db_row_selection == "all") {
                return(Inf) # Read all rows
            } else if (input$db_row_selection == "custom") {
                return(input$db_custom_rows)
            } else {
                return(as.numeric(input$db_row_selection))
            }
        }
    }

    # Observe tab changes
    observeEvent(input$data_source, {
        rv$data_source <- input$data_source
        # Reset data when switching sources
        rv$df <- NULL
        output$data_preview <- renderDT(NULL)
        shinyjs::disable("convert")
        add_log(paste("Switched to data source:", input$data_source))
    })

    # Handle database connection
    observeEvent(input$connect_db, {
        req(input$db_host, input$db_port, input$db_username, input$db_name, input$db_table)

        add_log("Attempting to connect to PostgreSQL database...")

        tryCatch(
            {
                # Connect to PostgreSQL
                rv$db_conn <- dbConnect(
                    Postgres(),
                    host = input$db_host,
                    port = input$db_port,
                    user = input$db_username,
                    password = input$db_password,
                    dbname = input$db_name
                )

                add_log(paste("Successfully connected to database:", input$db_name))
                output$status <- renderText("Database connected. Click 'Preview Data' to see table.")

                # Enable preview button
                shinyjs::enable("preview")
            },
            error = function(e) {
                add_log(paste("Database connection failed:", e$message))
                output$status <- renderText("Failed to connect to database.")
            }
        )
    })

    # Observe file upload (for file source)
    observeEvent(input$file, {
        req(input$file)
        file <- input$file
        ext <- tolower(file_ext(file$name))

        add_log(paste("File uploaded:", file$name))
        output$status <- renderText("Reading file...")

        if (!file.exists(file$datapath)) {
            add_log("File path does not exist.")
            output$status <- renderText("Error: File path does not exist.")
            return()
        }

        num_rows <- get_num_rows()
        add_log(paste("Reading", ifelse(is.infinite(num_rows), "all", num_rows), "rows of the file."))
        add_log(paste("File exists at path:", file$datapath))

        df <- tryCatch(
            {
                if (ext %in% c("xlsx", "xls")) {
                    add_log("Attempting to read Excel file with readxl...")
                    if (is.infinite(num_rows)) {
                        readxl::read_excel(file$datapath)
                    } else {
                        readxl::read_excel(file$datapath, n_max = num_rows)
                    }
                } else if (ext == "csv") {
                    add_log("Attempting to read CSV file with readr...")
                    if (is.infinite(num_rows)) {
                        readr::read_csv(file$datapath, col_types = cols())
                    } else {
                        readr::read_csv(file$datapath, n_max = num_rows, col_types = cols())
                    }
                } else if (ext %in% c("rda", "rdata")) {
                    add_log("Attempting to read RDA file...")
                    # Load RDA file into a temporary environment
                    env <- new.env()
                    load(file$datapath, envir = env)
                    # Get the first object (assuming it's the data frame)
                    obj_names <- ls(env)
                    if (length(obj_names) == 0) {
                        stop("No objects found in RDA file.")
                    }
                    df <- get(obj_names[1], envir = env)
                    # Apply row limit if needed
                    if (!is.infinite(num_rows) && nrow(df) > num_rows) {
                        df <- df[1:num_rows, ]
                    }
                    df
                } else if (ext == "rds") {
                    add_log("Attempting to read RDS file...")
                    df <- readRDS(file$datapath)
                    # Apply row limit if needed
                    if (!is.infinite(num_rows) && nrow(df) > num_rows) {
                        df <- df[1:num_rows, ]
                    }
                    df
                } else {
                    stop("Unsupported file type.")
                }
            },
            error = function(e) {
                add_log(paste("Error with file reading:", e$message))
                return(NULL)
            }
        )

        if (is.null(df)) {
            output$status <- renderText("Failed to read file.")
            shinyjs::disable("preview")
            return()
        }

        # Verify the loaded object is a data frame
        if (!is.data.frame(df)) {
            add_log(paste("Loaded object is not a data frame. Type:", class(df)))
            output$status <- renderText("Error: The file doesn't contain a data frame.")
            shinyjs::disable("preview")
            return()
        }

        rv$df <- df
        add_log(paste("Successfully read", nrow(df), "rows from file."))
        add_log(paste("Data structure:", paste(names(df), collapse = ", ")))
        output$status <- renderText("Preview ready. Click 'Preview Data' to see.")
        shinyjs::enable("preview")
        shinyjs::enable("convert") # Enable convert button since we have valid data
    })

    # Handle the Preview button click
    observeEvent(input$preview, {
        req(rv$df) # Make sure the data is available

        add_log("Displaying preview data.")

        # Create a display version of the data frame with GeoJSON handling
        display_df <- rv$df

        # Check if geojson column exists
        if ("geojson" %in% names(display_df)) {
display_df$geojson <- lapply(seq_len(nrow(display_df)), function(i) {
  x <- display_df$geojson[[i]]
  tryCatch({
    btn_id <- paste0("view_geojson_", i)
    json_str <- tryCatch({
      if (inherits(x, "geo_list")) {
        jsonlite::toJSON(unclass(x), auto_unbox = TRUE)
      } else if (is.list(x)) {
        jsonlite::toJSON(x, auto_unbox = TRUE)
      } else {
        jsonlite::toJSON(list(data = x), auto_unbox = TRUE)
      }
    }, error = function(e) {
      jsonlite::toJSON(list(error = e$message), auto_unbox = TRUE)
    })
    
    as.character(
      actionButton(
        btn_id,
        "View GeoJSON",
        onclick = sprintf('Shiny.setInputValue("view_geojson", %s, {priority: "event"}); Shiny.setInputValue("view_geojson_btn", "%s", {priority: "event"})',
        json_str,
        btn_id
      )
    )
    )
  }, error = function(e) {
    as.character(paste("Error creating button:", e$message))
  })
})
        }

        # Render the preview table with pagination
        output$data_preview <- renderDT({
            datatable(
                display_df,
                options = list(
                    pageLength = 10,
                    autoWidth = TRUE,
                    dom = "lftip"
                ),
                escape = FALSE, # Allow HTML rendering
                rownames = FALSE
            )
        })

        # Enable the convert button after preview
        shinyjs::enable("convert")
    })

# Enhanced GeoJSON viewer with property display
observeEvent(input$view_geojson, {
    tryCatch(
        {
            # Parse the GeoJSON data
            geojson_data <- if (is.character(input$view_geojson)) {
                jsonlite::fromJSON(input$view_geojson)
            } else {
                input$view_geojson
            }

            # Get the row index from the button ID
            btn_id <- if (!is.null(input$view_geojson_btn)) input$view_geojson_btn else NULL
            row_index <- if (!is.null(btn_id)) as.integer(gsub("view_geojson_", "", btn_id)) else NULL

            # Get additional properties from the data frame if available
            additional_props <- if (!is.null(row_index) && !is.null(rv$df)) {
                list(
                    NAME = if ("setting" %in% names(rv$df)) rv$df$setting[row_index] else NULL,
                    iso3_code = if ("iso3" %in% names(rv$df)) rv$df$iso3[row_index] else NULL
                )
            } else {
                NULL
            }

            # Extract properties if available
            properties <- NULL
            if (!is.null(geojson_data$properties)) {
                properties <- geojson_data$properties
            } else if (!is.null(geojson_data$features) && length(geojson_data$features) > 0) {
                # Get properties from first feature if it's a FeatureCollection
                properties <- geojson_data$features[[1]]$properties
            }

            # Merge with additional properties
            if (!is.null(additional_props)) {
                properties <- c(properties, additional_props[!sapply(additional_props, is.null)])
            }

            # Create property table if properties exist
            prop_table <- if (!is.null(properties)) {
                tags$div(
                    h4("Properties"),
                    renderTable({
                        # Flatten any nested structures
                        flat_props <- lapply(properties, function(x) {
                            if (is.list(x) || length(x) > 1) {
                                jsonlite::toJSON(x, auto_unbox = TRUE)
                            } else {
                                x
                            }
                        })

                        # Convert to data frame safely
                        data.frame(
                            Property = names(flat_props),
                            Value = unlist(lapply(flat_props, function(x) if (is.null(x)) NA else x)),
                            stringsAsFactors = FALSE
                        )
                    })
                )
            }

            showModal(
                modalDialog(
                    title = "GeoJSON Data Viewer",
                    fluidRow(
                        column(
                            6,
                            h4("GeoJSON Structure"),
                            tags$pre(
                                style = "white-space: pre-wrap; word-wrap: break-word; max-height: 60vh; overflow-y: auto;",
                                jsonlite::toJSON(geojson_data, pretty = TRUE, auto_unbox = TRUE)
                            )
                        ),
                        column(
                            6,
                            prop_table,
                            h4("Feature Type"),
                            verbatimTextOutput("geojsonTypeOutput")
                        )
                    ),
                    size = "l",
                    easyClose = TRUE,
                    footer = modalButton("Close")
                )
            )

            # Output the GeoJSON type
            output$geojsonTypeOutput <- renderText({
                if (!is.null(geojson_data$type)) {
                    geojson_data$type
                } else if (!is.null(geojson_data$features)) {
                    paste("FeatureCollection with", length(geojson_data$features), "features")
                } else {
                    "Unknown GeoJSON type"
                }
            })
        },
        error = function(e) {
            showModal(
                modalDialog(
                    title = "Error",
                    paste("Could not display GeoJSON:", e$message),
                    easyClose = TRUE
                )
            )
        }
    )
})

    # Handle the Convert button click
    observeEvent(input$convert, {
        req(rv$df) # Make sure the data is available
        req(input$output_format) # Make sure output format is selected

        if (rv$data_source == "PostgreSQL") {
            add_log("Starting conversion from PostgreSQL...")
            source_name <- paste("table", input$db_table, "from database", input$db_name)
        } else {
            add_log("Starting conversion from file...")
            source_name <- paste("file", input$file$name)
        }

        # Use Shiny's built-in progress bar to show progress
        withProgress(message = "Converting data...", value = 0, {
            incProgress(0.2)

            # Create output filename based on source and format
            if (rv$data_source == "PostgreSQL") {
                base_name <- paste0(
                    input$db_name, "_", input$db_table, "_",
                    format(Sys.time(), "%Y%m%d_%H%M%S")
                )
            } else {
                base_name <- tools::file_path_sans_ext(input$file$name)
            }

            # Determine file extension and conversion function
            output_ext <- switch(input$output_format,
                "parquet" = ".parquet",
                "csv" = ".csv",
                "excel" = ".xlsx",
                "json" = ".json"
            )

            output_path <- file.path(tempdir(), paste0(base_name, output_ext))

            tryCatch(
                {
                    # Create a copy of the data frame for conversion
                    df_to_convert <- rv$df

                    # Special handling for GeoJSON columns if they exist
                    if ("geojson" %in% names(df_to_convert)) {
                        df_to_convert$geojson <- lapply(df_to_convert$geojson, function(x) {
                            if (is.null(x)) {
                                return(NA)
                            }
                            if (length(x) == 0) {
                                return(NA)
                            }
                            if (inherits(x, "geo_list")) {
                                return(unclass(x))
                            }
                            return(x)
                        })
                    }

                    switch(input$output_format,
                        "parquet" = {
                            arrow::write_parquet(df_to_convert, output_path)
                            add_log(paste("Successfully converted to Parquet:", output_path))
                        },
                        "csv" = {
                            # For CSV, convert complex objects to JSON strings
                            df_to_convert[] <- lapply(df_to_convert, function(col) {
                                if (is.list(col)) {
                                    sapply(col, function(x) {
                                        if (is.null(x) || identical(x, NA)) {
                                            return(NA)
                                        }
                                        jsonlite::toJSON(x, auto_unbox = TRUE)
                                    })
                                } else {
                                    col
                                }
                            })
                            readr::write_csv(df_to_convert, output_path)
                            add_log(paste("Successfully converted to CSV:", output_path))
                        },
                        "excel" = {
                            # For Excel, convert complex objects to JSON strings
                            df_to_convert[] <- lapply(df_to_convert, function(col) {
                                if (is.list(col)) {
                                    sapply(col, function(x) {
                                        if (is.null(x) || identical(x, NA)) {
                                            return(NA)
                                        }
                                        jsonlite::toJSON(x, auto_unbox = TRUE)
                                    })
                                } else {
                                    col
                                }
                            })
                            openxlsx::write.xlsx(df_to_convert, output_path)
                            add_log(paste("Successfully converted to Excel:", output_path))
                        },
                        "json" = {
                            jsonlite::write_json(df_to_convert, output_path, auto_unbox = TRUE)
                            add_log(paste("Successfully converted to JSON:", output_path))
                        }
                    )

                    rv$converted_file <- output_path
                    output$status <- renderText(paste(
                        "Conversion to",
                        toupper(input$output_format),
                        "complete."
                    ))
                    incProgress(1.0)
                },
                error = function(e) {
                    add_log(paste("Error during conversion:", e$message))
                    output$status <- renderText(paste("Failed to convert:", e$message))
                }
            )
        })
    })

    # Download handler for the converted file
    output$download_output <- downloadHandler(
        filename = function() {
            if (!is.null(rv$converted_file)) {
                basename(rv$converted_file)
            } else {
                paste0(
                    "converted_", Sys.Date(),
                    switch(input$output_format,
                        "parquet" = ".parquet",
                        "csv" = ".csv",
                        "excel" = ".xlsx",
                        "json" = ".json"
                    )
                )
            }
        },
        content = function(file) {
            req(rv$converted_file)
            file.copy(rv$converted_file, file)
        }
    )

    # Add output to check if geojson column exists
    output$geojson_exists <- reactive({
        !is.null(rv$df) && "geojson" %in% names(rv$df)
    })
    outputOptions(output, "geojson_exists", suspendWhenHidden = FALSE)

    # Enhanced handler for GeoJSON download with setting and iso3 integration
    output$download_geojson <- downloadHandler(
        filename = function() {
            if (rv$data_source == "PostgreSQL") {
                paste0(input$db_table, "_geojson_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".geojson")
            } else {
                paste0(tools::file_path_sans_ext(input$file$name), "_geojson.geojson")
            }
        },
        content = function(file) {
            req(rv$df, "geojson" %in% names(rv$df))

            tryCatch(
                {
                    # Combine all features from all rows
                    all_features <- unlist(lapply(seq_len(nrow(rv$df)), function(i) {
                        x <- rv$df$geojson[[i]]
                        setting_name <- if ("setting" %in% names(rv$df)) rv$df$setting[i] else NULL
                        iso3_code <- if ("iso3" %in% names(rv$df)) rv$df$iso3[i] else NULL

                        if (inherits(x, "geo_list")) {
                            features <- unclass(x)$features
                        } else if (is.list(x) && !is.null(x$features)) {
                            features <- x$features
                        } else if (is.list(x)) {
                            features <- list(x) # Assume it's a single feature
                        } else {
                            return(NULL)
                        }

                        # Process each feature to ensure it has NAME and iso3_code properties
                        lapply(features, function(feat) {
                            if (is.list(feat$properties)) {
                                if (!is.null(setting_name) && is.null(feat$properties$NAME)) {
                                    feat$properties$NAME <- setting_name
                                }
                                if (!is.null(iso3_code) && is.null(feat$properties$iso3_code)) {
                                    feat$properties$iso3_code <- iso3_code
                                }
                            }
                            feat
                        })
                    }), recursive = FALSE)

                    # Remove NULL features
                    all_features <- Filter(Negate(is.null), all_features)

                    if (length(all_features) == 0) {
                        stop("No valid GeoJSON features found")
                    }

                    # Create FeatureCollection
                    feature_collection <- list(
                        type = "FeatureCollection",
                        features = all_features
                    )

                    # Write to temporary file
                    tmp_file <- tempfile(fileext = ".geojson")
                    writeLines(jsonlite::toJSON(feature_collection, auto_unbox = TRUE, pretty = TRUE), tmp_file)

                    # Copy to download
                    file.copy(tmp_file, file)

                    add_log("Successfully generated GeoJSON with setting names and ISO3 codes integrated")
                },
                error = function(e) {
                    showNotification(paste("Error generating GeoJSON:", e$message), type = "error")
                    add_log(paste("Error generating GeoJSON:", e$message))
                    return(NULL)
                }
            )
        }
    )
}

# Run the app
shinyApp(ui = ui, server = server)