library(shiny)
library(shinydashboard) # for Dashboard
library(shinyWidgets) # for radio button widgets
library(shinydashboardPlus)
library(shinyjs) # to perform common useful JavaScript operations in Shiny apps
library(shinyBS) # for bsTooltip function
library(shinyalert) # for alert message very nice format
library(dplyr) # select functions are covered in the require
library(plyr) # empty() function is from this package
library(DT) # for using %>% which works as a pipe in R code
library(ggplot2)
library(plotly)
library(scales) ## used to format date like only month or month and year
library(colorspace) # to generate Rainbow coloring function
library(pastecs) # for descriptive statistics
library(shinycssloaders)
library(gridlayout)
library(bslib)
library(colourpicker)
library(httr)
library(jsonlite)
library(openxlsx)
library(arrow) # Add arrow library for Parquet support


ui <- fluidPage(
    titlePanel("Data Cleaning Dashboard"),
    sidebarLayout(
        sidebarPanel(
            #actionButton("load", "Load Data"),
            #hr(),
            selectInput("na_action", "Handle Missing Values:",
                choices = c("None", "Remove Rows", "Replace with Mean", "Replace with Median", "Replace with Mode")
            ),
            actionButton("apply_na", "Apply"),
            hr(),
            checkboxInput("remove_dupes", "Remove Duplicates", FALSE),
            actionButton("apply_dupes", "Apply"),
            hr(),
            uiOutput("col_select"),
            selectInput("convert_type", "Convert Data Type:",
                choices = c("None", "Numeric", "Character", "Factor", "Date")
            ),
            actionButton("apply_convert", "Apply"),
            hr(),
            uiOutput("rename_ui"),
            actionButton("apply_rename", "Apply Rename"),
            hr(),
            numericInput("zscore_threshold", "Z-score Outlier Threshold", value = 3, min = 1, max = 10, step = 0.1),
            actionButton("detect_outliers", "Detect Outliers"),
            hr(),
            actionButton("save", "Save Cleaned Data"),
            downloadButton("download", "Download Cleaned Data")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Data Preview", DTOutput("table")),
                tabPanel("Outlier Detection", DTOutput("outliers"))
            )
        )
    )
)

server <- function(input, output, session) {
    data <- reactiveVal()

    # Automatically load data when the app starts
    observe({
        if (file.exists("fetched_data/main.rds")) {
            df <- readRDS("fetched_data/main.rds")
            data(df)
        } else {
            showNotification("No data file found. Please fetch data first!", type = "error")
        }
    })

    # Display the loaded data in the Data Preview tab
    output$table <- renderDT({
        req(data()) # Ensure data is available
        datatable(data(), options = list(scrollX = TRUE))
    })

    # Handle missing values based on user selection
    observeEvent(input$apply_na, {
        df <- data()
        if (input$na_action == "Remove Rows") {
            df <- na.omit(df)
        } else if (input$na_action == "Replace with Mean") {
            df <- df %>% mutate(across(where(is.numeric), ~ replace_na(., mean(., na.rm = TRUE))))
        } else if (input$na_action == "Replace with Median") {
            df <- df %>% mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))))
        } else if (input$na_action == "Replace with Mode") {
            mode_fn <- function(x) as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
            df <- df %>% mutate(across(where(is.numeric), ~ replace_na(., mode_fn(.))))
        }
        data(df)
    })

    # Remove duplicates if the checkbox is selected
    observeEvent(input$apply_dupes, {
        if (input$remove_dupes) {
            data(data() %>% distinct())
        }
    })

    # Create column selection UI dynamically
    output$col_select <- renderUI({
        req(data())
        selectInput("col", "Select Column:", choices = names(data()), selected = NULL)
    })

    # Convert column data type based on user selection
    observeEvent(input$apply_convert, {
        req(input$col)
        df <- data()
        if (input$convert_type == "Numeric") {
            df[[input$col]] <- as.numeric(df[[input$col]])
        } else if (input$convert_type == "Character") {
            df[[input$col]] <- as.character(df[[input$col]])
        } else if (input$convert_type == "Factor") {
            df[[input$col]] <- as.factor(df[[input$col]])
        } else if (input$convert_type == "Date") {
            df[[input$col]] <- as.Date(df[[input$col]])
        }
        data(df)
    })

    # Generate UI for renaming columns
    output$rename_ui <- renderUI({
        req(data())
        tagList(
            lapply(names(data()), function(col) {
                textInput(paste0("rename_", col), paste("Rename", col), col)
            })
        )
    })

    # Apply column renaming
    observeEvent(input$apply_rename, {
        df <- data()
        new_names <- map_chr(names(df), ~ input[[paste0("rename_", .)]])
        names(df) <- new_names
        data(df)
    })

    # Detect outliers based on Z-score threshold
    observeEvent(input$detect_outliers, {
        df <- data()
        num_cols <- df %>% select(where(is.numeric))
        outlier_df <- map_dfr(num_cols, function(col) {
            z_scores <- scale(col)
            df[abs(z_scores) > input$zscore_threshold, ]
        })
        output$outliers <- renderDT({
            datatable(outlier_df)
        })
    })

    # Save the cleaned data locally
    observeEvent(input$save, {
        saveRDS(data(), "fetched_data/main.rds")
    })

    # Download the cleaned data
    output$download <- downloadHandler(
        filename = "cleaned_data.rds",
        content = function(file) {
            saveRDS(data(), file)
        }
    )
}

#shinyApp(ui, server)
#shinyApp(ui = ui, server = server)