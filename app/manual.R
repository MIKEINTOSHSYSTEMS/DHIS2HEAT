# manual.R - Enhanced Interactive User Manual Module with Admin Features

# Required Libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(writexl)
library(pagedown)
library(readxl)
library(purrr)
library(stringr)
library(rmarkdown)
library(markdown)
library(htmltools)
library(tidyverse)

# Module UI Component
manualUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        useShinyjs(),
        tags$head(
            tags$script(src = "./www/js/tinymce.js", referrerpolicy = "origin"),
            tags$script("Shiny.addCustomMessageHandler('getTinyContent',
              function(message) {
                var content = tinymce.activeEditor.getContent();
                Shiny.onInputChange('tinyMCE_content', content);
              })"),
            tags$link(rel = "stylesheet", type = "text/css", href = "manual.css")
        ),
        div(
            class = "manual-container",
            div(
                class = "manual-header",
                h2(icon("book"), "User Manual"),
                p("DHIS2 Data Fetcher for HEAT+")
            ),

            # Search and filter section
            div(
                class = "search-container",
                fluidRow(
                    column(
                        width = 6,
                        searchInput(
                            ns("manual_search"),
                            #label = NULL,
                            label = "Search Manual",
                            placeholder = "Search manual by title, content or tags...",
                            btnSearch = icon("search"),
                            btnReset = icon("remove"),
                            width = "100%"
                        )
                    ),
                    column(
                        width = 3,
                        selectInput(
                            ns("filter_category"),
                            "Filter by Category",
                            #placeholder = "Filter By Categories",
                            choices = NULL,
                            multiple = TRUE,
                            width = "100%"
                        )
                    ),
                    column(
                        width = 3,
                        selectInput(
                            ns("filter_section"),
                            "Filter by Section",
                            #placeholder = "Filter By Sections",
                            choices = NULL,
                            multiple = TRUE,
                            width = "100%"
                        )
                    )
                )
            ),

            # Main content area
            fluidRow(
                column(
                    width = 3,
                    div(
                        class = "toc-container",
                        h4("Table of Contents"),
                        uiOutput(ns("toc_list"))
                    )
                ),
                column(
                    width = 9,
                    uiOutput(ns("manual_content")),
                    conditionalPanel(
                        condition = "output.is_admin",
                        ns = ns,
                        div(
                            class = "admin-tools",
                            h3("Admin Tools"),
                            fluidRow(
                                column(
                                    width = 6,
                                    actionButton(ns("add_section"), "Add New Section", icon = icon("plus")),
                                    actionButton(ns("add_content"), "Add New Content", icon = icon("plus"))
                                ),
                                column(
                                    width = 6,
                                    fileInput(ns("manual_upload"), "Upload Excel Manual", accept = ".xlsx"),
                                    downloadButton(ns("manual_download"), "Download Current Manual"),
                                    downloadButton(ns("export_pdf"), "Export to PDF"),
                                    actionButton(ns("refresh_manual"), "Refresh Content", icon = icon("sync"))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}

# Module Server Component
manualServer <- function(id, user_role) {
    moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns

            # Debugging - print current working directory
            print(paste("Current working directory:", getwd()))
            print(list.files(recursive = TRUE))

            # Reactive value for manual content
            manual_data <- reactiveVal()
            filtered_data <- reactiveVal()

            # Improved load_manual_data function with robust error handling
load_manual_data <- function() {
    tryCatch(
        {
            data_dir <- "./data/"
            data_file <- file.path(data_dir, "manual_content.xlsx")

            # Create data directory if it doesn't exist
            if (!dir.exists(data_dir)) {
                dir.create(data_dir)
                showNotification("Created data directory", type = "message")
            }

            if (file.exists(data_file)) {
                df <- read_excel(data_file) %>%
                    mutate(
                        id = as.numeric(id),
                        order = as.numeric(order)
                    ) %>%
                    arrange(order)

                # Convert section to factor with ordered levels
                section_order <- df %>%
                    group_by(section) %>%
                    summarise(min_order = min(order)) %>%
                    arrange(min_order) %>%
                    pull(section)

                df <- df %>%
                    mutate(section = factor(section, levels = section_order))

                # Check for required columns
                required_cols <- c("id", "section", "title", "content", "tags", "category", "order")
                missing_cols <- setdiff(required_cols, colnames(df))

                if (length(missing_cols) > 0) {
                    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
                }

                manual_data(df)
                filtered_data(df)
                showNotification("Manual data loaded successfully", type = "message")
            } else {
                # Create default data if file doesn't exist
                df <- data.frame(
                    id = 1:4,
                    section = c("Introduction", "Data Management", "Visualization", "System Admin"),
                    title = c("Welcome to DHIS2 Data Fetcher for HEAT+", "Data Handling Guide", "Creating Charts", "System Administration"),
                    content = c("Initial content...", "Data content...", "Visualization content...", "System admin content..."),
                    tags = c("overview", "data,guide", "charts", "system,admin"),
                    category = c("General", "Data", "Visual", "System"),
                    order = 1:4,
                    stringsAsFactors = FALSE
                )

                # Ensure the data directory exists
                if (!dir.exists(data_dir)) {
                    dir.create(data_dir)
                }

                write_xlsx(df, data_file)
                manual_data(df)
                filtered_data(df)
                showNotification("Created new manual data file", type = "message")
            }
        },
        error = function(e) {
            # Create empty dataframe with correct structure if error occurs
            df <- data.frame(
                id = numeric(),
                section = character(),
                title = character(),
                content = character(),
                tags = character(),
                category = character(),
                order = numeric(),
                stringsAsFactors = FALSE
            )
            manual_data(df)
            filtered_data(df)
            showNotification(paste("Error loading manual:", e$message), type = "error")
    

                    # Return empty data frame with correct structure if error occurs
                    df <- data.frame(
                        id = numeric(),
                        section = character(),
                        title = character(),
                        content = character(),
                        tags = character(),
                        category = character(),
                        order = numeric(),
                        stringsAsFactors = FALSE
                    )
                    manual_data(df)
                    filtered_data(df)
                })
            }

            # Initial load
            load_manual_data()

            # Update filter choices based on data
            observe({
                req(manual_data())
                df <- manual_data()

                updateSelectInput(
                    session, "filter_category",
                    choices = unique(df$category),
                    selected = NULL
                )

                updateSelectInput(
                    session, "filter_section",
                    choices = unique(df$section),
                    selected = NULL
                )
            })

            # Filter data based on search and filters
            observe({
                req(manual_data())
                df <- manual_data()

                # Apply search filter
                if (!is.null(input$manual_search) && input$manual_search != "") {
                    search_term <- tolower(input$manual_search)
                    df <- df %>%
                        filter(
                            grepl(search_term, tolower(title)) |
                                grepl(search_term, tolower(content)) |
                                grepl(search_term, tolower(tags))
                        )
                }

                # Apply category filter
                if (!is.null(input$filter_category)) {
                    df <- df %>% filter(category %in% input$filter_category)
                }

                # Apply section filter
                if (!is.null(input$filter_section)) {
                    df <- df %>% filter(section %in% input$filter_section)
                }

                filtered_data(df)
            })

            # Generate table of contents
            output$toc_list <- renderUI({
                req(filtered_data())
                df <- filtered_data()

                sections <- unique(df$section)

                tagList(
                    map(sections, function(section) {
                        section_id <- str_replace_all(tolower(section), " ", "-")
                        section_items <- df %>% filter(section == !!section)

                        div(
                            h5(section),
                            map(seq_len(nrow(section_items)), function(i) {
                                item <- section_items[i, ]
                                item_id <- paste0(section_id, "_", item$id)

                                div(
                                    class = "toc-item",
                                    onclick = paste0("document.getElementById('", item_id, "').scrollIntoView({behavior: 'smooth'});"),
                                    item$title
                                )
                            })
                        )
                    })
                )
            })

            # Render manual content
            output$manual_content <- renderUI({
                req(filtered_data())
                df <- filtered_data()
                sections <- split(df, df$section)

                tagList(
                    map(sections, function(section_df) {
                        section_name <- unique(section_df$section)
                        section_id <- str_replace_all(tolower(section_name), " ", "-")

                        div(
                            id = section_id,
                            class = "manual-section",
                            #h3(icon("folder-open"), section_name),
                            h3(icon("book-open"), section_name),
                            map(seq_len(nrow(section_df)), function(i) {
                                item <- section_df[i, ]
                                item_id <- paste0(section_id, "_", item$id)

                                div(
                                    id = item_id,
                                    class = "manual-card",
                                    div(
                                        class = "manual-card-header",
                                        div(
                                            class = "section-title",
                                            if (user_role() == "admin") {
                                                span(class = "sortable-handle", icon("grip-vertical"))
                                            },
                                            h4(item$title)
                                        ),
                                        if (user_role() == "admin") {
                                            actionButton(ns(paste0("edit_", item_id)), "Edit",
                                                class = "btn-xs btn-primary"
                                            )
                                        }
                                    ),
                                    div(
                                        class = "manual-card-body",
                                        HTML(markdown::markdownToHTML(text = item$content, fragment.only = TRUE)),
                                        div(
                                            class = "manual-tags",
                                            map(strsplit(item$tags, ",")[[1]], function(tag) {
                                                span(class = "manual-tag", tag)
                                            })
                                        )
                                    )
                                )
                            })
                        )
                    })
                )
            })

            output$is_admin <- reactive({
                req(user_role())
                user_role() == "admin"
            })
            outputOptions(output, "is_admin", suspendWhenHidden = FALSE)

            # Download current manual as Excel
            output$manual_download <- downloadHandler(
                filename = function() {
                    paste0("heat_manual_", format(Sys.time(), "%Y%m%d"), ".xlsx")
                },
                content = function(file) {
                    write_xlsx(manual_data(), path = file)
                }
            )

            # Handle manual upload
            observeEvent(input$manual_upload, {
                req(input$manual_upload)
                tryCatch(
                    {
                        df <- read_excel(input$manual_upload$datapath)
                        
                        # Ensure all required columns exist
                        required_cols <- c("id", "section", "title", "content", "tags", "category", "order")
                        missing_cols <- setdiff(required_cols, colnames(df))
                        
                        if (length(missing_cols) > 0) {
                            stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
                        }
                        
                        # Ensure proper data types
                        df <- df %>%
                            mutate(
                                id = as.numeric(id),
                                order = as.numeric(order)
                            )
                        
                        manual_data(df)
                        write_xlsx(df, "./data/manual_content.xlsx")
                        showNotification("Manual updated successfully!", type = "message")
                    },
                    error = function(e) {
                        showNotification(paste("Upload failed:", e$message), type = "error")
                    }
                )
            })

            # Export to PDF
#####################################            
output$export_pdf <- downloadHandler(
    filename = "heat_manual.pdf",
    content = function(file) {
        # Show initial notification
        showNotification("Preparing PDF generation...", duration = NULL, id = "pdf_progress")

        # Check and install TinyTeX if needed
        if (!requireNamespace("tinytex", quietly = TRUE)) {
            showNotification("Installing tinytex package...", duration = NULL)
            install.packages("tinytex", quiet = TRUE)
        }

        # Ensure TinyTeX is properly installed with all packages
        tryCatch(
            {
                if (!tinytex::is_tinytex()) {
                    showNotification("Installing TinyTeX (this may take several minutes)...",
                        duration = NULL, id = "install_tex"
                    )
                    tinytex::install_tinytex()
                    removeNotification(id = "install_tex")
                }

                # Install required LaTeX packages with specific repository
                showNotification("Checking LaTeX packages...", id = "pdf_progress")

                # Set a reliable repository
                tinytex::tlmgr_repo("https://mirror.ctan.org/systems/texlive/tlnet")

                required_pkgs <- c(
                    "geometry", "hyperref", "ulem", "amsmath", "lineno", "fancyhdr",
                    "titling", "sectsty", "titlesec", "booktabs", "longtable", "multirow",
                    "wrapfig", "float", "colortbl", "pdflscape", "threeparttable",
                    "xcolor", "graphicx", "setspace", "footmisc", "caption", "parskip"
                )

                # Install packages one by one with error handling
                for (pkg in required_pkgs) {
                    tryCatch(
                        {
                            if (!pkg %in% tinytex::tl_pkgs()) {
                                tinytex::tlmgr_install(pkg)
                            }
                        },
                        error = function(e) {
                            message("Failed to install package: ", pkg, " - ", e$message)
                        }
                    )
                }
            },
            error = function(e) {
                removeNotification(id = "pdf_progress")
                showNotification(
                    paste(
                        "LaTeX setup failed:", e$message,
                        "\nPlease try manual installation: tinytex::install_tinytex()"
                    ),
                    type = "error",
                    duration = 10
                )
                return()
            }
        )

        # Create temporary directory
        temp_dir <- tempdir()
        temp_report <- file.path(temp_dir, "manual_report.Rmd")
        temp_log <- file.path(temp_dir, "latex_log.txt")

        # Function to properly escape LaTeX special characters
        escape_latex <- function(x) {
            if (is.na(x)) {
                return("")
            }
            # First replace backslashes with a temporary marker
            x <- gsub("\\", "TEMP_BACKSLASH", x, fixed = TRUE)
            # Then escape other special characters
            x <- gsub("&", "\\&", x, fixed = TRUE)
            x <- gsub("%", "\\%", x, fixed = TRUE)
            x <- gsub("$", "\\$", x, fixed = TRUE)
            x <- gsub("#", "\\#", x, fixed = TRUE)
            x <- gsub("_", "\\_", x, fixed = TRUE)
            x <- gsub("{", "\\{", x, fixed = TRUE)
            x <- gsub("}", "\\}", x, fixed = TRUE)
            x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
            x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
            # Finally replace the temporary marker with proper LaTeX backslash
            x <- gsub("TEMP_BACKSLASH", "\\textbackslash{}", x, fixed = TRUE)
            x
        }

        # Function to convert markdown to LaTeX-safe text
        md_to_latex <- function(x) {
            # First escape special LaTeX characters
            x <- escape_latex(x)

            # Convert basic markdown to LaTeX
            x <- gsub("\\*\\*(.*?)\\*\\*", "\\\\textbf{\\1}", x)
            x <- gsub("\\*(.*?)\\*", "\\\\textit{\\1}", x)
            x <- gsub("`(.*?)`", "\\\\texttt{\\1}", x)
            x <- gsub("\\[(.*?)\\]\\((.*?)\\)", "\\\\href{\\2}{\\1}", x)

            # Handle lists - convert to itemize environment
            x <- strsplit(x, "\n")[[1]] # Split by lines
            in_list <- FALSE
            result <- character()

            for (line in x) {
                if (grepl("^\\s*[-*+]\\s+", line) || grepl("^\\s*\\d+\\.\\s+", line)) {
                    if (!in_list) {
                        result <- c(result, "\\begin{itemize}")
                        in_list <- TRUE
                    }
                    line <- gsub("^\\s*[-*+]\\s+(.*)", "\\\\item \\1", line)
                    line <- gsub("^\\s*\\d+\\.\\s+(.*)", "\\\\item \\1", line)
                    result <- c(result, line)
                } else {
                    if (in_list) {
                        result <- c(result, "\\end{itemize}")
                        in_list <- FALSE
                    }
                    result <- c(result, line)
                }
            }

            if (in_list) {
                result <- c(result, "\\end{itemize}")
            }

            paste(result, collapse = "\n")
        }

        # Prepare the data with proper content sanitization
        manual_df <- tryCatch(
            {
                df <- manual_data()
                if (!is.null(df)) {
                    df %>%
                        mutate(across(everything(), as.character)) %>%
                        mutate(
                            title = map_chr(title, escape_latex),
                            section = map_chr(section, escape_latex),
                            tags = map_chr(tags, escape_latex),
                            content = map_chr(content, md_to_latex)
                        )
                }
            },
            error = function(e) {
                showNotification("Error preparing manual content", type = "error")
                NULL
            }
        )

        # Create a simplified RMarkdown template
        template_content <- paste(
            "---",
            'title: "DHIS2 Data Fetcher for HEAT+ User Manual"',
            'author: "HEAT+Dev Team"',
            'date: "`r format(Sys.Date(), \'%B %d, %Y\')`"',
            "output:",
            "  pdf_document:",
            "    toc: true",
            "    toc_depth: 3",
            "    number_sections: true",
            "    latex_engine: xelatex",
            "geometry: margin=1in",
            "header-includes:",
            "  - \\usepackage{booktabs}",
            "  - \\usepackage{longtable}",
            "  - \\usepackage[table]{xcolor}",
            "  - \\usepackage{parskip}",
            "  - \\usepackage{enumitem}",
            "params:",
            "  manual_data: NULL",
            "---",
            "",
            "```{r setup, include=FALSE}",
            "library(knitr)",
            "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
            "options(knitr.kable.NA = '')",
            "```",
            "",
            "```{r load-data, include=FALSE}",
            "# Safely load data",
            "manual_df <- tryCatch({",
            "  if (!is.null(params$manual_data)) {",
            "    df <- as.data.frame(params$manual_data)",
            "    df",
            "  }",
            "}, error = function(e) NULL)",
            "```",
            "",
            "\\begin{document}",
            "\\maketitle",
            "",
            "```{r generate-content, results='asis'}",
            "if (!is.null(manual_df) && nrow(manual_df) > 0) {",
            "  sections <- unique(manual_df$section)",
            "  for (section in sections) {",
            "    cat(paste0('\\\\section{', section, '}\\\\n\\\\n'))",
            "    section_data <- manual_df[manual_df$section == section, ]",
            "    section_data <- section_data[order(as.numeric(section_data$order)), ]",
            "    ",
            "    for (i in seq_len(nrow(section_data))) {",
            "      item <- section_data[i, ]",
            "      cat(paste0('\\\\subsection{', item$title, '}\\\\n\\\\n'))",
            "      cat(item$content)",
            "      if (!is.na(item$tags) && nzchar(item$tags)) {",
            "        cat(paste0('\\\\n\\\\n\\\\textbf{Tags:} ', item$tags, '\\\\n'))",
            "      }",
            "      cat('\\\\n\\\\n')",
            "    }",
            "  }",
            "} else {",
            "  cat('No manual content available.')",
            "}",
            "```",
            "",
            "\\end{document}",
            sep = "\n"
        )

        # Write the template
        writeLines(template_content, temp_report)

        # Generate PDF with detailed error handling
        tryCatch(
            {
                showNotification("Generating PDF document...", id = "pdf_progress")

                # Capture LaTeX output for debugging
                latex_output <- capture.output(
                    {
                        rmarkdown::render(
                            input = temp_report,
                            output_file = file,
                            params = list(manual_data = manual_df),
                            envir = new.env(parent = globalenv()),
                            quiet = FALSE,
                            encoding = "UTF-8"
                        )
                    },
                    type = "message"
                )

                writeLines(latex_output, temp_log)

                removeNotification(id = "pdf_progress")
                showNotification("PDF generated successfully!", type = "message", duration = 5)
            },
            error = function(e) {
                removeNotification(id = "pdf_progress")

                # Write detailed error log
                error_log <- c(
                    paste("Error:", e$message),
                    "\nLaTeX log:",
                    if (file.exists(temp_log)) readLines(temp_log) else "No log available",
                    "\nTraceback:",
                    capture.output(traceback())
                )

                error_log_file <- file.path(temp_dir, "pdf_error_log.txt")
                writeLines(error_log, error_log_file)

                # User-friendly error message
                error_msg <- if (grepl("Undefined control sequence", e$message)) {
                    paste(
                        "PDF generation failed due to special characters in content.",
                        "The system has tried to escape them, but some may need manual adjustment.",
                        "Full error log saved to:", error_log_file
                    )
                } else if (grepl("xelatex", e$message)) {
                    paste(
                        "PDF generation failed: LaTeX compilation error.",
                        "Common causes:",
                        "1. Missing LaTeX packages - try: tinytex::tlmgr_install(c('xcolor', 'geometry', 'hyperref'))",
                        "2. Special characters in content that need escaping",
                        "Full error log saved to:", error_log_file
                    )
                } else {
                    paste("PDF generation failed:", e$message, "\nSee log:", error_log_file)
                }

                showNotification(error_msg, type = "error", duration = 15)
            }
        )
    }
)
#######################################
            # Refresh manual content
            observeEvent(input$refresh_manual, {
                load_manual_data()
            })

            # Add new section
            observeEvent(input$add_section, {
                showModal(
                    modalDialog(
                        title = "Add New Section",
                        textInput(ns("new_section_name"), "Section Name", placeholder = "e.g., Advanced Features"),
                        footer = tagList(
                            modalButton("Cancel"),
                            actionButton(ns("confirm_add_section"), "Add Section")
                        )
                    )
                )
            })

            observeEvent(input$confirm_add_section, {
                req(input$new_section_name)

                current_data <- manual_data()
                new_id <- if (nrow(current_data) == 0) 1 else max(current_data$id, na.rm = TRUE) + 1
                new_order <- if (nrow(current_data) == 0) 1 else max(current_data$order, na.rm = TRUE) + 1

                new_section <- data.frame(
                    id = new_id,
                    section = input$new_section_name,
                    title = "New Content Item",
                    content = "Enter content here...",
                    tags = "new",
                    category = "General",
                    order = new_order,
                    stringsAsFactors = FALSE
                )

                updated_data <- bind_rows(current_data, new_section)
                manual_data(updated_data)
                write_xlsx(updated_data, "./data/manual_content.xlsx")
                removeModal()
                showNotification("New section added successfully!", type = "message")
            })

            # Add new content
            observeEvent(input$add_content, {
                showModal(
                    modalDialog(
                        title = "Add New Content",
                        selectInput(ns("content_section"), "Select Section", choices = unique(manual_data()$section)),
                        textInput(ns("content_title"), "Title", placeholder = "Content title"),
                        selectInput(ns("content_category"), "Category", choices = unique(manual_data()$category)),
                        selectizeInput(ns("content_tags"), "Tags",
                            choices = unique(unlist(strsplit(manual_data()$tags, ","))),
                            multiple = TRUE
                        ),
                        footer = tagList(
                            modalButton("Cancel"),
                            actionButton(ns("confirm_add_content"), "Add Content")
                        )
                    )
                )
            })

            observeEvent(input$confirm_add_content, {
                req(input$content_section, input$content_title)

                current_data <- manual_data()
                new_id <- if (nrow(current_data) == 0) 1 else max(current_data$id, na.rm = TRUE) + 1
                new_order <- if (nrow(current_data) == 0) 1 else max(current_data$order, na.rm = TRUE) + 1

                new_content <- data.frame(
                    id = new_id,
                    section = input$content_section,
                    title = input$content_title,
                    content = "Enter content here...",
                    tags = paste(input$content_tags, collapse = ","),
                    category = input$content_category,
                    order = new_order,
                    stringsAsFactors = FALSE
                )

                updated_data <- bind_rows(current_data, new_content)
                manual_data(updated_data)
                write_xlsx(updated_data, "./data/manual_content.xlsx")
                removeModal()
                showNotification("New content added successfully!", type = "message")
            })

            # Edit content
            observe({
                req(user_role() == "admin")
                lapply(names(input), function(input_name) {
                    if (str_detect(input_name, "^edit_")) {
                        observeEvent(input[[input_name]], {
                            item_id <- str_remove(input_name, "edit_")
                            df <- manual_data() %>%
                                mutate(temp_id = paste0(str_replace_all(tolower(section), " ", "-"), "_", id))
                            item <- df %>% filter(temp_id == item_id)

                            if (nrow(item) == 1) {
                                showModal(
                                    modalDialog(
                                        title = "Edit Manual Content",
                                        size = "l",
                                        textInput(ns("edit_title"), "Title", value = item$title),
                                        selectInput(ns("edit_section"), "Section",
                                            choices = unique(manual_data()$section),
                                            selected = item$section
                                        ),
                                        selectInput(ns("edit_category"), "Category",
                                            choices = unique(manual_data()$category),
                                            selected = item$category
                                        ),
                                        selectizeInput(ns("edit_tags"), "Tags",
                                            choices = unique(unlist(strsplit(manual_data()$tags, ","))),
                                            selected = strsplit(item$tags, ",")[[1]],
                                            multiple = TRUE
                                        ),
                                        numericInput(ns("edit_order"), "Display Order", value = item$order),
                                        div(
                                            id = ns("editor-container"),
                                            HTML(paste0(
                                                '<textarea id="', ns("tinyMCE_editor"), '" style="height:300px;">',
                                                item$content, "</textarea>"
                                            ))
                                        ),
                                        footer = tagList(
                                            modalButton("Cancel"),
                                            actionButton(ns("save_edit"), "Save Changes"),
                                            actionButton(ns("delete_content"), "Delete", class = "btn-danger pull-left")
                                        ),
                                        tags$script(paste0('
                                            setTimeout(function() {
                                                tinymce.init({
                                                    selector: "#', ns("tinyMCE_editor"), '",
                                                    menubar: false,
                                                    branding: false,
                                                    plugins: "lists table link code",
                                                    contextmenu: "lists link table",
                                                    toolbar1: "bold italic forecolor backcolor | formatselect fontselect fontsizeselect | alignleft aligncenter alignright alignjustify",
                                                    toolbar2: "undo redo removeformat bullist numlist table blockquote code superscript subscript strikethrough link",
                                                    height: 300,
                                                    setup: function(editor) {
                                                        editor.on("change", function() {
                                                            tinymce.triggerSave();
                                                        });
                                                    }
                                                });
                                            }, 100);
                                        '))
                                    )
                                )
                            }
                        })
                    }
                })
            })

            # Save edited content
            observeEvent(input$save_edit, {
                req(input$tinyMCE_editor)
                
                edit_button <- names(which(sapply(names(input), function(x) str_detect(x, "^edit_"))))
                item_id <- str_remove(edit_button, "edit_")

                updated_data <- manual_data() %>%
                    mutate(temp_id = paste0(str_replace_all(tolower(section), " ", "-"), "_", id)) %>%
                    mutate(
                        section = ifelse(temp_id == item_id, input$edit_section, section),
                        title = ifelse(temp_id == item_id, input$edit_title, title),
                        content = ifelse(temp_id == item_id, input$tinyMCE_editor, content),
                        tags = ifelse(temp_id == item_id, paste(input$edit_tags, collapse = ","), tags),
                        category = ifelse(temp_id == item_id, input$edit_category, category),
                        order = ifelse(temp_id == item_id, input$edit_order, order)
                    ) %>%
                    select(-temp_id)

                manual_data(updated_data)
                write_xlsx(updated_data, "./data/manual_content.xlsx")
                
                runjs("if (tinymce.activeEditor) { tinymce.activeEditor.remove(); }")
                removeModal()
                showNotification("Changes saved successfully!", type = "message")
            })

            # Delete content
            observeEvent(input$delete_content, {
                edit_button <- names(which(sapply(names(input), function(x) str_detect(x, "^edit_"))))
                item_id <- str_remove(edit_button, "edit_")

                showModal(
                    modalDialog(
                        title = "Confirm Deletion",
                        "Are you sure you want to delete this content item?",
                        footer = tagList(
                            modalButton("Cancel"),
                            actionButton(ns("confirm_delete"), "Delete", class = "btn-danger")
                        )
                    )
                )
            })

            observeEvent(input$confirm_delete, {
                edit_button <- names(which(sapply(names(input), function(x) str_detect(x, "^edit_"))))
                item_id <- str_remove(edit_button, "edit_")

                updated_data <- manual_data() %>%
                    mutate(temp_id = paste0(str_replace_all(tolower(section), " ", "-"), "_", id)) %>%
                    filter(temp_id != item_id) %>%
                    select(-temp_id)

                manual_data(updated_data)
                write_xlsx(updated_data, "./data/manual_content.xlsx")
                removeModal()
                showNotification("Content deleted successfully!", type = "message")
            })

            # Make sections sortable when admin
            observe({
                if (req(user_role()) == "admin") {
                    shinyjs::runjs("
                        $(function() {
                          $('.manual-section').sortable({
                            handle: '.sortable-handle',
                            update: function(event, ui) {
                              Shiny.setInputValue('manual_order', $(this).sortable('toArray').toString());
                            }
                          });
                        });
                    ")
                }
            })

            # Handle reordering
            observeEvent(input$manual_order, {
                req(input$manual_order)
                order_ids <- strsplit(input$manual_order, ",")[[1]]
                current_data <- manual_data()

                updated_data <- current_data %>%
                    mutate(
                        order = match(
                            paste0(str_replace_all(tolower(section), " ", "-"), "-", id),
                            order_ids
                        )
                    ) %>%
                    arrange(order)

                manual_data(updated_data)
                write_xlsx(updated_data, "./data/manual_content.xlsx")
            })
        }
    )
}

# Standalone App for Testing
manualApp <- function() {
    # Create data directory if it doesn't exist
    if (!dir.exists("./data")) {
        dir.create("./data")
    }

    # Create template RMarkdown file if missing
    if (!file.exists("manual_template.Rmd")) {
        template_content <- paste(
            "---",
            'title: "DHIS2 Data Fetcher for HEAT+ User Manual"',
            "output:",
            "  pdf_document:",
            "    toc: true",
            "    toc_depth: 3",
            "params:",
            "  manual_data:",
            "    section: [\"\"]",
            "    title: [\"\"]",
            "    content: [\"\"]",
            "    tags: [\"\"]",
            "    category: [\"\"]",
            "    order: [1]",
            "---",
            "",
            "```{r setup, include=FALSE}",
            "library(tidyverse)",
            "library(knitr)",
            "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
            "```",
            "",
            "# DHIS2 Data Fetcher for HEAT+ User Manual",
            "",
            "```{r generate-content, results='asis'}",
            "if (!is.null(params$manual_data)) {",
            "  sections <- unique(params$manual_data$section)",
            "  ",
            "  for (section in sections) {",
            "    cat(paste0('\\\\n\\\\n## ', section, '\\\\n\\\\n'))",
            "    ",
            "    section_data <- params$manual_data %>% ",
            "      filter(section == !!section) %>% ",
            "      arrange(order)",
            "    ",
            "    for (i in 1:nrow(section_data)) {",
            "      item <- section_data[i, ]",
            "      cat(paste0('\\\\n\\\\n### ', item$title, '\\\\n\\\\n'))",
            "      cat(item$content)",
            "      cat(paste0('\\\\n\\\\n**Tags:** ', item$tags, '\\\\n'))",
            "    }",
            "  }",
            "}",
            "```",
            sep = "\n"
        )
        writeLines(template_content, "manual_template.Rmd")
    }

    ui <- fluidPage(
        useShinyjs(),
        manualUI("manual")
    )

    server <- function(input, output, session) {
        manualServer("manual", reactive("admin"))
    }

    shinyApp(ui, server)
}

# Run the app in interactive mode
if (interactive()) {
    manualApp()
}