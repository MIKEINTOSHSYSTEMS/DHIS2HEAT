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

ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs to control button enablement

  titlePanel("Excel/CSV to Parquet Converter (with PostgreSQL Support)"),
  sidebarLayout(
    sidebarPanel(
      # Tabset for different data sources
      tabsetPanel(
        id = "data_source",
        tabPanel(
          "File Upload",
          fileInput("file", "Upload Excel or CSV File",
            accept = c(".xlsx", ".xls", ".csv")
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
      actionButton("convert", "Convert to Parquet", disabled = TRUE),
      br(),
      textOutput("status"),
      br(),
      downloadButton("download_parquet", "Download Parquet")
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
  # Reactive values to store logs, file data, and parquet file info
  rv <- reactiveValues(
    log = "",
    parquet_file = NULL,
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
    ext <- file_ext(file$name)

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
      return()
    }

    rv$df <- df
    add_log(paste("Successfully read", nrow(df), "rows from file."))
    output$status <- renderText("Preview ready. Click 'Preview Data' to see.")
    shinyjs::enable("preview")
  })

  # Handle the Preview button click
  observeEvent(input$preview, {
    if (rv$data_source == "file") {
      req(rv$df) # For file source
    } else {
      req(rv$db_conn, input$db_table) # For database source
    }

    add_log("Displaying preview data.")

    if (rv$data_source == "PostgreSQL") {
      # Read data from PostgreSQL
      output$status <- renderText("Reading data from database...")

      tryCatch(
        {
          # Get the number of rows to preview
          num_rows <- get_num_rows()

          # Construct the query
          if (is.infinite(num_rows)) {
            query <- paste("SELECT * FROM", input$db_table)
            add_log("Reading all rows from database table.")
          } else {
            query <- paste("SELECT * FROM", input$db_table, "LIMIT", num_rows)
            add_log(paste("Reading", num_rows, "rows from database table."))
          }

          rv$df <- dbGetQuery(rv$db_conn, query)

          add_log(paste("Successfully read", nrow(rv$df), "rows from table:", input$db_table))
          output$status <- renderText("Database data loaded. Ready to convert.")
        },
        error = function(e) {
          add_log(paste("Error reading from database:", e$message))
          output$status <- renderText("Failed to read from database.")
          return()
        }
      )
    }

    # Render the preview table with pagination
    output$data_preview <- renderDT({
      datatable(rv$df, options = list(pageLength = 10, autoWidth = TRUE))
    })

    # Enable the convert button after preview
    shinyjs::enable("convert")
  })

  # Handle the Convert button click
  observeEvent(input$convert, {
    req(rv$df) # Make sure the data is available

    if (rv$data_source == "PostgreSQL") {
      add_log("Starting conversion from PostgreSQL to Parquet...")
      source_name <- paste("table", input$db_table, "from database", input$db_name)
    } else {
      add_log("Starting conversion from file to Parquet...")
      source_name <- paste("file", input$file$name)
    }

    # Use Shiny's built-in progress bar to show progress
    withProgress(message = "Converting data...", value = 0, {
      incProgress(0.2)

      # Create output filename based on source
      if (rv$data_source == "PostgreSQL") {
        parquet_path <- file.path(
          tempdir(),
          paste0(
            input$db_name, "_", input$db_table, "_",
            format(Sys.time(), "%Y%m%d_%H%M%S"), ".parquet"
          )
        )
      } else {
        parquet_path <- file.path(
          tempdir(),
          paste0(
            tools::file_path_sans_ext(input$file$name),
            "_converted.parquet"
          )
        )
      }

      tryCatch(
        {
          write_parquet(rv$df, parquet_path)
          rv$parquet_file <- parquet_path
          add_log(paste("Successfully converted", source_name, "to Parquet:", parquet_path))
          output$status <- renderText("Conversion complete.")
          incProgress(1.0)
        },
        error = function(e) {
          add_log(paste("Error writing parquet:", e$message))
          output$status <- renderText("Failed to convert.")
        }
      )
    })
  })

  # Download handler for the converted file
  output$download_parquet <- downloadHandler(
    filename = function() {
      if (!is.null(rv$parquet_file)) {
        basename(rv$parquet_file)
      } else {
        paste0("converted_", Sys.Date(), ".parquet")
      }
    },
    content = function(file) {
      req(rv$parquet_file)
      file.copy(rv$parquet_file, file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)