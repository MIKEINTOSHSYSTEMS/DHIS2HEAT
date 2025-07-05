library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(DT)
library(shinyWidgets)
# library(shinythemes)


# Define UI for the module
dba_module_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        useShinyjs(),
        includeCSS("./www/styles.css"),
        titlePanel("Database Administration"),
        fluidRow( # Changed to fluidRow for full width
            column(
                width = 12, # Full width column
                div(
                    class = "panel panel-primary",
                    div(
                        class = "panel-heading",
                        h4("Backup Management", class = "panel-title")
                    ),
                    div(
                        class = "panel-body",
                        passwordInput(ns("password"), "Enter Admin Password:"),
                        actionBttn(ns("backup"), "Create Backup", style = "material-flat", color = "primary"),
                        br(),
                        DTOutput(ns("backup_files")),
                        fluidRow(
                            column(4, actionBttn(ns("download_backup"), "Download Selected", style = "material-flat", color = "success")),
                            column(4, actionBttn(ns("restore"), "Restore Selected", style = "material-flat", color = "warning")),
                            column(4, actionBttn(ns("delete"), "Delete Selected", style = "material-flat", color = "danger"))
                        ),
                        verbatimTextOutput(ns("status"))
                    )
                ),
                div(
                    class = "panel panel-primary",
                    div(
                        class = "panel-heading",
                        h4("Database Management", class = "panel-title")
                    ),
                    div(
                        class = "panel-body",
                        pickerInput(ns("table_select"), "Select Table:", choices = NULL, options = list(`live-search` = TRUE)),
                        fluidRow(
                            column(6, actionBttn(ns("refresh_tables"), "Refresh Tables", style = "material-flat", color = "primary")),
                            column(6, actionBttn(ns("delete_table"), "Delete Table", style = "material-flat", color = "danger"))
                        ),
                        DTOutput(ns("table_content")),
                        fluidRow(
                            column(4, actionBttn(ns("add_row"), "Add Row", style = "material-flat", color = "primary")),
                            column(4, actionBttn(ns("edit_row"), "Edit Row", style = "material-flat", color = "warning")),
                            column(4, actionBttn(ns("delete_row"), "Delete Row", style = "material-flat", color = "danger"))
                        ),
                        verbatimTextOutput(ns("db_status"))
                    )
                )
            )
        )
    )
}


# Define server logic for the module
dba_module_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        backup_dir <- "dbackup"
        dir.create(backup_dir, showWarnings = FALSE)
    
    # Reactive value to trigger updates
    backup_trigger <- reactiveVal(0)
    
    # Password validation
    valid_password <- reactive({
      input$password == "dhis2heat" # Replace with your admin password
    })
    
    observe({
      shinyjs::toggleState("backup", valid_password())
      shinyjs::toggleState("download_backup", valid_password() && length(input$backup_files_rows_selected) == 1)
      shinyjs::toggleState("restore", valid_password() && length(input$backup_files_rows_selected) == 1)
      shinyjs::toggleState("delete", valid_password() && length(input$backup_files_rows_selected) > 0)
    })
    
    # Backup functions
    backup_files <- reactive({
      backup_trigger()
      files <- list.files(backup_dir, pattern = "\\.sqlite$", full.names = FALSE)
      data.frame(
        Files = files,
        Size = file.size(file.path(backup_dir, files)),
        Date = file.info(file.path(backup_dir, files))$mtime
      )
    })
    
    output$backup_files <- renderDT({
      datatable(backup_files(), selection = "multiple", options = list(pageLength = 5))
    })
    
    # Backup handlers
    observeEvent(input$backup, {
      if (valid_password()) {
        backup_file <- paste0(backup_dir, "/data_backup_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".sqlite")
        file.copy("./db/data.sqlite", backup_file)
        backup_trigger(backup_trigger() + 1)
        showNotification("Backup created successfully!", type = "message")
      }
    })
    
    output$download_backup <- downloadHandler(
      filename = function() {
        backup_files()[input$backup_files_rows_selected, "Files"]
      },
      content = function(file) {
        backup_file <- file.path(backup_dir, backup_files()[input$backup_files_rows_selected, "Files"])
        file.copy(backup_file, file)
      }
    )
    
    observeEvent(input$restore, {
      if (valid_password() && length(input$backup_files_rows_selected) == 1) {
        backup_file <- file.path(backup_dir, backup_files()[input$backup_files_rows_selected, "Files"])
        file.copy(backup_file, "./db/data.sqlite", overwrite = TRUE)
        backup_trigger(backup_trigger() + 1)
        showNotification("Database restored successfully! Please restart the application.", type = "message")
      }
    })
    
    observeEvent(input$delete, {
      if (valid_password() && length(input$backup_files_rows_selected) > 0) {
        files_to_delete <- file.path(backup_dir, backup_files()[input$backup_files_rows_selected, "Files"])
        file.remove(files_to_delete)
        backup_trigger(backup_trigger() + 1)
        showNotification("Selected backups deleted!", type = "message")
      }
    })

        backup_files <- reactive({
            # Depend on backup trigger
            backup_trigger()

            files <- list.files(backup_dir, pattern = "data_backup_.*\\.sqlite", full.names = FALSE)
            data.frame(Files = files, Date = as.POSIXct(sub("data_backup_|\\.sqlite", "", files), format = "%Y-%m-%d_%H-%M-%S"))
        })

        updateBackupFiles <- function() {
            output$backup_files <- renderDT({
                datatable(backup_files(), selection = "multiple", options = list(pageLength = 5, order = list(list(2, "desc"))))
            })
        }

        updateBackupFiles()

        # Database Management
        conn <- dbConnect(RSQLite::SQLite(), "./db/data.sqlite")

        observeEvent(input$refresh_tables, {
            updateTableChoices()
        })

        updateTableChoices <- function() {
            tables <- dbListTables(conn)
            updatePickerInput(session, "table_select", choices = tables)
        }

        observeEvent(input$table_select, {
            updateTableContent()
        })

        updateTableContent <- function() {
            req(input$table_select)
            table_data <- dbReadTable(conn, input$table_select)
            output$table_content <- renderDT({
                datatable(table_data, selection = "single", editable = TRUE)
            })
        }

        observeEvent(input$add_table, {
            showModal(modalDialog(
                title = "Add Table",
                textInput("new_table_name", "Table Name:"),
                textInput("new_table_columns", "Columns (name type, e.g., id INTEGER, name TEXT):"),
                footer = tagList(
                    modalButton("Cancel"),
                    actionBttn("create_table", "Create Table", style = "material-flat", color = "primary")
                )
            ))
        })

        observeEvent(input$create_table, {
            table_name <- input$new_table_name
            columns <- input$new_table_columns
            dbExecute(conn, paste0("CREATE TABLE ", table_name, " (", columns, ")"))
            updateTableChoices()
            removeModal()
            output$db_status <- renderText("Table created successfully.")
        })

        observeEvent(input$delete_table, {
            req(input$table_select)
            dbExecute(conn, paste0("DROP TABLE ", input$table_select))
            updateTableChoices()
            output$db_status <- renderText("Table deleted successfully.")
        })

        observeEvent(input$add_row, {
            req(input$table_select)
            showModal(modalDialog(
                title = "Add Row",
                textInput("new_row_values", "Values (comma-separated):"),
                footer = tagList(
                    modalButton("Cancel"),
                    actionBttn("insert_row", "Insert Row", style = "material-flat", color = "primary")
                )
            ))
        })

        observeEvent(input$insert_row, {
            req(input$table_select)
            values <- input$new_row_values
            dbExecute(conn, paste0("INSERT INTO ", input$table_select, " VALUES (", values, ")"))
            updateTableContent()
            removeModal()
            output$db_status <- renderText("Row added successfully.")
        })

        observeEvent(input$delete_row, {
            req(input$table_select)
            selected_row <- input$table_content_rows_selected
            if (length(selected_row) == 1) {
                table_data <- dbReadTable(conn, input$table_select)
                row_id <- table_data[selected_row, 1]
                dbExecute(conn, paste0("DELETE FROM ", input$table_select, " WHERE rowid = ", row_id))
                updateTableContent()
                output$db_status <- renderText("Row deleted successfully.")
            } else {
                output$db_status <- renderText("Please select a single row to delete.")
            }
        })

        observeEvent(input$edit_row, {
            req(input$table_select)
            selected_row <- input$table_content_rows_selected
            if (length(selected_row) == 1) {
                table_data <- dbReadTable(conn, input$table_select)
                row_data <- table_data[selected_row, ]
                showModal(modalDialog(
                    title = "Edit Row",
                    textInput("edit_row_values", "Values (comma-separated):", value = paste(row_data, collapse = ", ")),
                    footer = tagList(
                        modalButton("Cancel"),
                        actionBttn("update_row", "Update Row", style = "material-flat", color = "primary")
                    )
                ))
            } else {
                output$db_status <- renderText("Please select a single row to edit.")
            }
        })

        observeEvent(input$update_row, {
            req(input$table_select)
            selected_row <- input$table_content_rows_selected
            table_data <- dbReadTable(conn, input$table_select)
            row_id <- table_data[selected_row, 1]
            values <- input$edit_row_values
            columns <- names(table_data)
            set_clause <- paste(paste(columns, values, sep = " = "), collapse = ", ")
            dbExecute(conn, paste0("UPDATE ", input$table_select, " SET ", set_clause, " WHERE rowid = ", row_id))
            updateTableContent()
            removeModal()
            output$db_status <- renderText("Row updated successfully.")
        })

        updateTableChoices()
    })
}
# Run the application
# shinyApp(ui = ui, server = server)