library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
# library(DT)
# library(shinythemes)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
    # theme = shinytheme("cerulean"),




    useShinyjs(),
    includeCSS("./www/custom.css"),
    titlePanel("Database Administration"),
    sidebarLayout(
        sidebarPanel(
            passwordInput("password", "Enter Password:"),
            actionBttn("backup", "Create Backup", style = "material-flat", color = "primary"),
            br(),
            DTOutput("backup_files"),
            actionBttn("restore", "Restore Selected Backup", style = "material-flat", color = "success"),
            actionBttn("delete", "Delete Selected Backups", style = "material-flat", color = "danger"),
            verbatimTextOutput("status"),
            hr(),
            h3("Database Management"),
            pickerInput("table_select", "Select Table:", choices = NULL, options = list(`live-search` = TRUE)),
            actionBttn("refresh_tables", "Refresh Tables", style = "material-flat", color = "primary"),
            actionBttn("add_table", "Add Table", style = "material-flat", color = "primary"),
            actionBttn("delete_table", "Delete Table", style = "material-flat", color = "danger"),
            DTOutput("table_content"),
            actionBttn("add_row", "Add Row", style = "material-flat", color = "primary"),
            actionBttn("delete_row", "Delete Selected Rows", style = "material-flat", color = "danger"),
            actionBttn("edit_row", "Edit Selected Row", style = "material-flat", color = "warning"),
            verbatimTextOutput("db_status")
        ),
        mainPanel(
            # Placeholder for additional UI elements if needed
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    backup_dir <- "dbackup"
    dir.create(backup_dir, showWarnings = FALSE)

    observeEvent(input$backup, {
        if (input$password == "dhis2heat") {
            backup_file <- paste0(backup_dir, "/data_backup_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".sqlite")
            file.copy("./db/data.sqlite", backup_file)
            output$status <- renderText("Backup created successfully.")
            updateBackupFiles()
        } else {
            output$status <- renderText("Incorrect password.")
        }
    })

    observeEvent(input$restore, {
        if (input$password == "dhis2heat") {
            selected_backup <- input$backup_files_rows_selected
            if (length(selected_backup) == 1) {
                backup_file <- backup_files()[selected_backup, "Files"]
                file.copy(file.path(backup_dir, backup_file), "./db/data.sqlite", overwrite = TRUE)
                output$status <- renderText("Backup restored successfully.")
            } else {
                output$status <- renderText("Please select a single backup to restore.")
            }
        } else {
            output$status <- renderText("Incorrect password.")
        }
    })

    observeEvent(input$delete, {
        if (input$password == "dhis2heat") {
            selected_backups <- input$backup_files_rows_selected
            if (length(selected_backups) > 0) {
                backup_files_to_delete <- backup_files()[selected_backups, "Files"]
                file.remove(file.path(backup_dir, backup_files_to_delete))
                output$status <- renderText("Selected backups deleted successfully.")
                updateBackupFiles()
            } else {
                output$status <- renderText("Please select backups to delete.")
            }
        } else {
            output$status <- renderText("Incorrect password.")
        }
    })

    backup_files <- reactive({
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
}

# Run the application
shinyApp(ui = ui, server = server)
