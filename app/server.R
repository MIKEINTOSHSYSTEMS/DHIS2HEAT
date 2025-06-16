library(shiny)
library(shinydashboard) # for Dashboard
library(shinyWidgets) # for radio button widgets
library(shinydashboardPlus)
library(shinyjs) # to perform common useful JavaScript operations in Shiny apps
library(shinyBS) # for bsTooltip function
library(shinyalert) # for alert message very nice format
library(RSQLite)
library(sodium) # For password hashing
# library(shinythemes)
library(plyr) # empty() function is from this package
library(dplyr) # select functions are covered in the require
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
library(openxlsx)
library(tidyr)
library(rjson)
library(jsonlite)
library(arrow) # Add arrow library for Parquet support
library(purrr)
library(here)
library(slickR)
library(rpivotTable) # For pivot table functionality
library(RPostgreSQL)
library(DBI)
library(viridisLite)
library(sf)
library(highcharter)

# Load existing functions
# source("dhis2_data.R")
# Source the data fetching functions only when needed
# source("dhis2_data.R", local = TRUE)
# source("dba.R", local = TRUE)$value
source("ethgeo.R") # not unless it is loading the geojson file
source("dba.R")
source("spatial_module.R")
# source("spatial.R")
# source("auth.R")

eth_geojson <- rjson::fromJSON(file = "./www/ethiopia_regions_map_simple.json")
mapboxToken <- "pk.eyJ1IjoiaG1vcmdhbnN0ZXdhcnQiLCJhIjoiY2tmaTg5NDljMDBwbDMwcDd2OHV6cnd5dCJ9.8eLR4FtlO079Gq0NeSNoeg"

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

options(future.globals.maxSize = 2 * 1024^3) # 2GB memory limit

# Define server logic
server <- function(input, output, session) {
  # Initialize reactive values
  data <- reactiveValues(
    combined = NULL,
    filtered = NULL
  )

  # Error Handling for JSON Parsing
  tryCatch(
    {
      indicators_metadata <- fromJSON("./meta/indicators.json")$indicators
      custom_indicators_metadata <- fromJSON("./meta/custom_indicators.json")$indicators
    },
    error = function(e) {
      showNotification(paste("Error loading indicator metadata:", e$message), type = "error")
    }
  )

  custom_indicators_metadata <- fromJSON("./meta/custom_indicators.json")$indicators
  indicators_metadata <- fromJSON("./meta/custom_indicators.json")$indicators
  # indicators_metadata <- fromJSON("./meta/indicators.json")$indicators
  zones_metadata <- fromJSON("./meta/organisationUnitsLevel3.json")$organisationUnits
  woredas_metadata <- fromJSON("./meta/organisationUnitsLevel4.json")$organisationUnits
  org_units_metadata <- fromJSON("./meta/organisationUnitsLevel2.json")$organisationUnits
  # specific_org_units <- fromJSON("./meta/organisationUnitsLevel2.json")$organisationUnits

  # Define specific organisation units (regions) server-side
  specific_org_units <- c(
    "yY9BLUUegel", "UFtGyqJMEZh", "yb9NKGA8uqt", "Fccw8uMlJHN",
    "tDoLtk2ylu4", "G9hDiPNoB7d", "moBiwh9h5Ce", "b9nYedsL8te",
    "XU2wpLlX4Vk", "xNUoZIrGKxQ", "PCKGSJoNHXi", "a2QIIR2UXcd",
    "HIlnt7Qj8do", "Gmw0DJLXGtx"
  )

  # checks if the indicator paths JSON are okay
  if (!file.exists("./meta/indicators.json")) {
    showNotification("indicators.json file not found at specified path", type = "error")
  }

  if (!file.exists("./meta/custom_indicators.json")) {
    showNotification("custom_indicators.json file not found at specified path", type = "error")
  }


  # Check Indicators Exist
  if (is.null(indicators_metadata)) {
    showNotification("Failed to load indicators metadata", type = "error")
  }
  if (is.null(custom_indicators_metadata)) {
    showNotification("Failed to load custom indicators metadata", type = "error")
  }


  # Content Slider
  output$content_slider <- renderSlickR({
    slickR(
      list(
        tags$div(tags$img(src = "assets/feature.jpeg"), h3("Welcome To"), p("DHIS2 Data Fetcher for HEAT")),
        tags$div(tags$img(src = "assets/feature1.jpeg"), h3("Live Data Fetch"), p("Realtime data fetch")),
        tags$div(tags$img(src = "assets/feature2.jpeg"), h3("Rich Visualizations"), p("Customize your visualizations and explore data")),
        tags$div(tags$img(src = "assets/feature3.jpeg"), h3("Comprehensive Data Management"), p("Explore, Search, Compare and Export InEquality Data"))
      )
    )
  })


  # Try to load data from main.rds on startup
  observe({
    if (file.exists("fetched_data/main.rds")) {
      tryCatch(
        {
          df <- readRDS("fetched_data/main.rds")
          if (!is.null(df) && nrow(df) > 0) {
            data$combined <- df
            data_fetched(TRUE)

            # Update filters based on loaded data
            updateSelectizeInput(session, "filter_indicators",
              choices = unique(data$combined$indicator_name)
            )
            updateSelectizeInput(session, "filter_dimensions",
              choices = unique(data$combined$dimension)
            )
            updateSelectizeInput(session, "filter_dates",
              choices = unique(data$combined$date)
            )

            showNotification("InEquality Data loaded from HEAT Server", type = "message")
          } else {
            showNotification("Loaded data file is empty", type = "warning")
          }
        },
        error = function(e) {
          showNotification(paste("Error loading data:", e$message), type = "error")
        }
      )
    } else {
      showNotification("No data file found. Please fetch data first", type = "warning")
    }
  })


  # Data Status Indicator:
  output$data_status <- renderUI({
    if (is.null(data$combined)) {
      tags$div(
        class = "alert alert-warning",
        "No data loaded. Please fetch data using the admin panel."
      )
    } else {
      tags$div(
        class = "alert alert-success",
        paste("Data loaded with", nrow(data$combined), "rows.")
      )
    }
  })



  # Define the validate_password function
  validate_password <- function(hashed_password, input_password) {
    sodium::password_verify(hashed_password, input_password)
  }

  # Define the hash_password function (if not already defined)
  hash_password <- function(password) {
    sodium::password_store(password)
  }


  observeEvent(input$toggle_sidebar, {
    toggleClass(selector = ".sidebar", class = "sidebar-hidden")
    toggleClass(selector = ".main", class = "main-expanded")
  })

  #  output$distPlot <- renderPlotly({
  # generate bins based on input$bins from ui.R
  #    plot_ly(x = ~ faithful[, 2], type = "histogram", marker = list(color = input$plot_color)) # Use selected color
  #  })

  #  output$bluePlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  #    x <- faithful[, 2]
  #    bins <- seq(min(x), max(x), length.out = input$bins + 1)

  # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = "steelblue", border = "white")
  # })

  #  output$data_preview <- renderDT({
  #    head(faithful, 10) # Adjust this to match your data
  #  })



  output$is_admin <- reactive({
    !is.null(user$info) && user$info$role == "admin"
  })
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)

  output$user_profile <- renderUI({
    if (user$logged_in) {
      tagList(
        div(
          class = "user-info",
          img(src = "images/user_picture.png", id = "user_profile_img"), # Correct image path
          div(
            class = "user-info-text",
            h4(user$info$username), # Dynamic username
            p(user$info$email) # Dynamic email or user role
          )
        ),
        div(
          class = "pdropdown-menu",
          div(class = "headerText", paste("Logged in as:", user$info$username)),
          notificationItem(
            text = actionLink("view_profile", "View Profile"),
            icon = icon("user")
          ),
          notificationItem(
            text = actionLink("change_password", "Change Password"),
            icon = icon("key")
          ),
          div(class = "divider"),
          notificationItem(
            text = actionLink("logout_btn", "Logout"),
            icon = icon("sign-out")
          )
        )
      )
    }
  })


  user <- reactiveValues(logged_in = FALSE, info = NULL, permissions = NULL)

  # Login/Register modals
  observeEvent(input$login_btn, showLoginModal())
  observeEvent(input$register_btn, showRegisterModal())

  showLoginModal <- function() {
    modalDialog(
      title = "Login",
      textInput("login_username", "Username"),
      passwordInput("login_password", "Password"),
      footer = tagList(
        actionButton("submit_login", "Login", class = "btn-primary"),
        modalButton("Cancel")
      )
    ) %>% showModal()
  }

  showRegisterModal <- function() {
    modalDialog(
      title = "Register",
      textInput("reg_username", "Username*"),
      passwordInput("reg_password", "Password*"),
      textInput("reg_email", "Email"),
      textInput("reg_fullname", "Full Name"),
      footer = tagList(
        actionButton("submit_register", "Register", class = "btn-success"),
        modalButton("Cancel")
      )
    ) %>% showModal()
  }

  # Login logic
  observeEvent(input$submit_login, {
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    user_data <- dbGetQuery(
      conn,
      "SELECT * FROM users WHERE username = ?",
      list(input$login_username)
    )
    dbDisconnect(conn)

    if (nrow(user_data) == 1 &&
      validate_password(user_data$password, input$login_password) &&
      user_data$is_active == 1) {
      user$logged_in <- TRUE
      user$info <- user_data
      removeModal()

      # Update last login
      conn <- dbConnect(SQLite(), "./db/data.sqlite")
      dbExecute(
        conn,
        "UPDATE users SET last_login = CURRENT_TIMESTAMP WHERE id = ?",
        list(user$info$id)
      )
      dbDisconnect(conn)
    } else {
      shinyalert("Login Failed", "Invalid credentials or inactive account", "error")
    }
  })

  # Registration logic
  observeEvent(input$submit_register, {
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    existing <- dbGetQuery(
      conn,
      "SELECT id FROM users WHERE username = ?",
      list(input$reg_username)
    )

    if (nrow(existing) > 0) {
      shinyalert("Registration Failed", "Username already exists", "error")
    } else {
      hashed_pw <- hash_password(input$reg_password)
      dbExecute(
        conn,
        "INSERT INTO users (username, password, email, full_name)
         VALUES (?, ?, ?, ?)",
        list(
          input$reg_username, hashed_pw,
          input$reg_email, input$reg_fullname
        )
      )
      shinyalert("Success", "Registration successful! Please login", "success")
      removeModal()
    }
    dbDisconnect(conn)
  })

  # Logout logic
  observeEvent(input$logout_btn, {
    user$logged_in <- FALSE
    user$info <- NULL
  })

  # Control UI based on authentication
  output$logged_in <- reactive(user$logged_in)
  outputOptions(output, "logged_in", suspendWhenHidden = FALSE)




  # Reactive expression to fetch user data
  user_table_data <- reactive({
    user_update_trigger() # Correctly reference the reactiveVal # Add a trigger to refresh the data
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    users <- dbGetQuery(
      conn,
      "SELECT id, username, email, full_name, role, is_active,
    strftime('%Y-%m-%d %H:%M', created_at) as created_at,
    strftime('%Y-%m-%d %H:%M', last_login) as last_login FROM users"
    )
    dbDisconnect(conn)
    users
  })

  # Add update trigger
  user_update_trigger <- reactiveVal(0)

  observeEvent(input$create_user, {
    user_update_trigger(user_update_trigger() + 1)
  })

  observeEvent(input$save_user, {
    user_update_trigger(user_update_trigger() + 1)
  })

  observeEvent(input$delete_user, {
    user_update_trigger(user_update_trigger() + 1)
  })

  # User Management Tab

  proxy <- dataTableProxy("user_table")

  observeEvent(user_table_data(), {
    replaceData(proxy, user_table_data(), resetPaging = FALSE)
  })

  output$user_table <- renderDT({
    datatable(user_table_data(),
      selection = "single",
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Edit user modal
  observeEvent(input$edit_user, {
    selected_row <- input$user_table_rows_selected
    if (is.null(selected_row)) {
      shinyalert("Error", "Please select a user to edit", "error")
      return()
    }

    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    user_data <- dbGetQuery(
      conn,
      "SELECT * FROM users WHERE id = ?",
      list(user_table_data()[selected_row, "id"])
    )
    dbDisconnect(conn)

    showModal(modalDialog(
      title = "Edit User",
      textInput("edit_username", "Username", value = user_data$username),
      textInput("edit_email", "Email", value = user_data$email),
      textInput("edit_fullname", "Full Name", value = user_data$full_name),
      selectInput("edit_role", "Role", choices = c("admin", "user"), selected = user_data$role),
      checkboxInput("edit_active", "Active", value = as.logical(user_data$is_active)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_user", "Save Changes", class = "btn-primary")
      )
    ))
  })

  # Save user changes
  observeEvent(input$save_user, {
    selected_row <- input$user_table_rows_selected
    if (is.null(selected_row)) {
      shinyalert("Error", "Please select a user to save", "error")
      return()
    }

    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    dbExecute(
      conn,
      "UPDATE users SET username = ?, email = ?, full_name = ?, role = ?, is_active = ? WHERE id = ?",
      list(
        input$edit_username, input$edit_email, input$edit_fullname,
        input$edit_role, as.integer(input$edit_active),
        user_table_data()[selected_row, "id"]
      )
    )
    dbDisconnect(conn)
    removeModal()
    shinyalert("Success", "User updated successfully", "success")
    user_update_trigger(user_update_trigger() + 1) # Trigger update
  })

  # Delete user
  observeEvent(input$delete_user, {
    selected_row <- input$user_table_rows_selected
    if (is.null(selected_row)) {
      shinyalert("Error", "Please select a user to delete", "error")
      return()
    }

    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    dbExecute(
      conn,
      "DELETE FROM users WHERE id = ?",
      list(user_table_data()[selected_row, "id"])
    )
    dbDisconnect(conn)
    shinyalert("Success", "User deleted successfully", "success")
    user_update_trigger(user_update_trigger() + 1) # Trigger update
  })



  # Role Management Tab
  output$role_table <- renderDT({
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    roles <- dbGetQuery(
      conn,
      "SELECT role_name, permissions FROM roles"
    )
    dbDisconnect(conn)

    datatable(roles,
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })

  observeEvent(input$add_role, {
    req(input$new_role_name)
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    tryCatch({
      dbExecute(
        conn,
        "INSERT INTO roles (role_name, permissions) VALUES (?, ?)",
        list(input$new_role_name, paste(input$new_role_perms, collapse = ","))
      )
      shinyalert("Success", "Role added successfully", "success")
    }, error = function(e) {
      shinyalert("Error", paste("Failed to add role:", e$message), "error")
    }, finally = {
      dbDisconnect(conn)
    })
  })

  observeEvent(input$delete_role, {
    req(input$role_select)
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    tryCatch({
      dbExecute(
        conn,
        "DELETE FROM roles WHERE role_name = ?",
        list(input$role_select)
      )
      shinyalert("Success", "Role deleted successfully", "success")
    }, error = function(e) {
      shinyalert("Error", paste("Failed to delete role:", e$message), "error")
    }, finally = {
      dbDisconnect(conn)
    })
  })

  # Update role selection input
  observe({
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    roles <- dbGetQuery(conn, "SELECT role_name FROM roles")
    dbDisconnect(conn)
    updateSelectInput(session, "role_select", choices = roles$role_name)
  })



  # Edit Profile Modal
  observeEvent(input$edit_profile, {
    showModal(modalDialog(
      title = "Edit Profile",
      textInput("edit_email", "Email", value = user$info$email),
      textInput("edit_fullname", "Full Name", value = user$info$full_name),
      footer = tagList(
        actionButton("save_profile", "Save Changes"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$view_profile, {
    showModal(modalDialog(
      title = "User Profile",
      tagList(
        div(
          class = "form-group",
          tags$label("Full Name:"),
          textInput("profile_fullname", NULL, value = user$info$full_name)
        ),
        div(
          class = "form-group",
          tags$label("Email:"),
          textInput("profile_email", NULL, value = user$info$email)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_profile", "Save Changes", class = "btn-primary")
        )
      )
    ))
  })

  observeEvent(input$save_profile, {
    req(user$logged_in)
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    tryCatch({
      dbExecute(
        conn,
        "UPDATE users SET full_name = ?, email = ? WHERE id = ?",
        list(input$profile_fullname, input$profile_email, user$info$id)
      )
      user$info$full_name <- input$profile_fullname
      user$info$email <- input$profile_email
      removeModal()
      shinyalert("Success", "Profile updated successfully", "success")
    }, error = function(e) {
      shinyalert("Error", paste("Update failed:", e$message), "error")
    }, finally = {
      dbDisconnect(conn)
    })
  })

  # Password change modal
  observeEvent(input$change_password, {
    showModal(modalDialog(
      title = "Change Password",
      tagList(
        passwordInput("current_password", "Current Password"),
        passwordInput("new_password", "New Password"),
        passwordInput("confirm_password", "Confirm New Password")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_password_change", "Change Password", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_password_change, {
    req(user$logged_in)
    tryCatch({
      if (input$new_password != input$confirm_password) {
        stop("New passwords do not match")
      }

      conn <- dbConnect(SQLite(), "./db/data.sqlite")
      user_data <- dbGetQuery(
        conn,
        "SELECT password FROM users WHERE id = ?",
        list(user$info$id)
      )

      if (!sodium::password_verify(user_data$password, input$current_password)) {
        stop("Current password is incorrect")
      }

      new_hash <- sodium::password_store(input$new_password)
      dbExecute(
        conn,
        "UPDATE users SET password = ? WHERE id = ?",
        list(new_hash, user$info$id)
      )

      removeModal()
      shinyalert("Success", "Password changed successfully", "success")
    }, error = function(e) {
      shinyalert("Error", e$message, "error")
    }, finally = {
      dbDisconnect(conn)
    })
  })

  # Reactive expression to fetch roles
  roles_data <- reactive({
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    roles <- dbGetQuery(conn, "SELECT * FROM roles")
    dbDisconnect(conn)
    roles
  })

  # Update role selection input
  observe({
    updateSelectInput(session, "role_select", choices = roles_data()$role_name)
  })

  # Save permissions for a role
  # Save permissions
  observeEvent(input$save_permissions, {
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    dbExecute(
      conn,
      "UPDATE roles SET permissions = ? WHERE role_name = ?",
      list(paste(input$permissions, collapse = ","), input$role_select)
    )
    dbDisconnect(conn)
    shinyalert("Success", "Permissions updated", "success")
  })

  # Render permissions UI based on selected role
  output$permissions_ui <- renderUI({
    req(input$role_select)
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    on.exit(dbDisconnect(conn))

    permissions <- dbGetQuery(
      conn,
      "SELECT permissions FROM roles WHERE role_name = ?",
      list(input$role_select)
    )

    selected_permissions <- if (nrow(permissions) > 0) {
      unlist(strsplit(permissions$permissions[1], ","))
    } else {
      character(0)
    }

    # Get all available permissions from a central source
    # available_perms <- c(
    #  "user_management", "role_management",
    #  "data_preview", "settings", "dash_explore_clean"
    # )

    available_permissions <- reactive({
      conn <- dbConnect(SQLite(), "./db/data.sqlite")
      on.exit(dbDisconnect(conn))
      perms <- dbGetQuery(conn, "SELECT permission_name FROM permissions")$permission_name
      unique(perms)
    })

    checkboxGroupInput("permissions", "Permissions",
      choices = available_permissions(),
      selected = selected_permissions
    )
  })

  #  checkboxGroupInput("permissions", "Permissions",
  #    choices = available_perms,
  #    selected = selected_permissions
  #  )
  # })

  # Add reactive trigger for role updates
  roles_updated <- reactiveVal(0)
  observeEvent(input$save_permissions, {
    roles_updated(roles_updated() + 1)
  })



  output$new_role_perms_ui <- renderUI({
    checkboxGroupInput("new_role_perms", "Permissions",
      choices = available_permissions()
    )
  })



  # Control UI based on user role
  observeEvent(user$logged_in, {
    if (user$logged_in) {
      # Fetch user permissions from roles table
      conn <- dbConnect(SQLite(), "./db/data.sqlite")
      permissions <- dbGetQuery(
        conn,
        "SELECT permissions FROM roles WHERE role_name = ?",
        list(user$info$role)
      )
      dbDisconnect(conn)

      if (nrow(permissions) > 0) {
        user_perms <- unlist(strsplit(permissions$permissions[1], ","))

        # Show tabs based on permissions
        if ("data_preview" %in% user_perms) showTab("main_nav", "data_preview")
        if ("settings" %in% user_perms) showTab("main_nav", "settings")
        if ("dash_explore_clean" %in% user_perms) showTab("main_nav", "dash_explore_clean")
        if (any(c("user_management", "role_management") %in% user_perms)) {
          showTab("main_nav", "admin_panel")
        }
      }
    } else {
      showTab("main_nav", "home")
    }
  })

  # Add role-based access control for admin features
  observeEvent(input$sidebar_menu, {
    if (!is.null(user$info$role) && user$info$role != "admin" && input$sidebar_menu %in% c("user_management", "role_management")) {
      showModal(modalDialog(
        title = "Access Denied",
        "You don't have permission to access this section",
        easyClose = TRUE
      ))
      updateTabItems(session, "sidebar_menu", selected = "data_preview")
    }
  })


  # Role Management Tab
  output$role_table <- renderDT({
    conn <- dbConnect(SQLite(), "./db/data.sqlite")
    roles <- dbGetQuery(
      conn,
      "SELECT role_name, permissions FROM roles"
    )
    dbDisconnect(conn)

    datatable(roles,
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })



  # Reactive values to store settings and data
  settings <- reactiveValues()
  data <- reactiveValues()
  choices <- reactiveValues()
  data_fetched <- reactiveVal(FALSE)
  user_role <- reactiveVal(NULL)
  exported_file_name <- reactiveVal(NULL)
  fetched_data <- reactiveVal(NULL) # Add this line to store fetched data

  spatialServer("spatial_module")
  # ethgeoServer("ethgeo_module") # main server function for ethgeo module
  ethgeoServer("ethgeo_module", reactive(data$filtered))
  dba_module_server("dba_module")
  # Load data from main.rds if it exists
  # if (file.exists("fetched_data/main.rds")) {
  #  data$combined <- readRDS("fetched_data/main.rds")
  #  data_fetched(TRUE)
  # }

  observe({
    if (file.exists("fetched_data/main.rds")) {
      df <- readRDS("fetched_data/main.rds")
      if (!is.null(df) && nrow(df) > 0) {
        data$combined <- df
        data_fetched(TRUE)
      } else {
        showNotification("Loaded data is empty!", type = "error")
      }
    } else {
      showNotification("No data file found. Please fetch data first!", type = "error")
    }
  })


  # Update filters from main.rds when data loads
  observe({
    req(data$combined)

    # Update filter choices from combined data
    updateSelectizeInput(session, "filter_indicators",
      choices = unique(data$combined$indicator_name)
    )

    updateSelectizeInput(session, "filter_dimensions",
      choices = unique(data$combined$dimension)
    )

    updateSelectizeInput(session, "filter_dates",
      choices = unique(data$combined$date)
    )

    # Reset subgroups when data changes
    updateSelectizeInput(session, "filter_subgroups", choices = character(0))
  })


  observeEvent(input$toggle_settings, {
    # Toggle the settings panel visibility
    toggleClass(selector = "#settings_panel", class = "settings-collapsed")
  })

  observe({
    # Load both standard and custom indicators
    all_indicators <- c(
      setNames(indicators_metadata$id, indicators_metadata$displayName),
      setNames(custom_indicators_metadata$id, custom_indicators_metadata$displayName)
    )

    updateSelectizeInput(session, "indicators",
      choices = all_indicators,
      server = TRUE
    )

    # Also update the custom indicators dropdown
    updateSelectizeInput(session, "custom_indicators",
      choices = all_indicators,
      server = TRUE
    )
  })


  # debugging to see what's being passed to the selectize input
  observe({
    print("Indicator choices:")
    print(head(setNames(indicators_metadata$id, indicators_metadata$displayName)))

    updateSelectizeInput(session, "indicators",
      choices = setNames(indicators_metadata$id, indicators_metadata$displayName),
      options = list(maxOptions = 1000)
      # server = TRUE
    )
  })

  #  observe({
  # source("dhis2_data.R", local = TRUE)
  #    indicator_choices <- setNames(indicators_metadata$id[indicators_metadata$id %in% input$indicators], indicators_metadata$displayName[indicators_metadata$id %in% input$indicators])
  # indicator_choices <- setNames(indicators_metadata$id, indicators_metadata$displayName)
  #   updateSelectizeInput(session, "custom_indicators", choices = indicator_choices, server = TRUE)
  # updateSelectizeInput(session, "indicators", choices = choices$indicators, server = TRUE)
  #  })

  output$custom_scales_ui <- renderUI({
    req(input$custom_indicators)
    lapply(input$custom_indicators, function(indicator) {
      display_name <- indicators_metadata$displayName[indicators_metadata$id == indicator]
      numericInput(paste0("scale_", indicator), paste("Scale for", display_name), value = 100, min = 1)
    })
  })

  observeEvent(input$fetch_data, {
    # Source the data fetching functions only when needed
    source("dhis2_data.R", local = TRUE)

    # Validation check for indicators and abbreviations
    if (length(input$indicators) != length(strsplit(input$indicator_abbr, ",")[[1]])) {
      showModal(modalDialog(
        title = "Error",
        "The number of indicators and abbreviations must be equal.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    Sys.setenv(DHIS2_BASE_URL = input$base_url)
    Sys.setenv(DHIS2_USERNAME = input$username)
    Sys.setenv(DHIS2_PASSWORD = input$password)

    if (length(input$org_units) == 0) {
      sendSweetAlert(session, title = "Error", text = "No organisation units selected.", type = "error")
      return()
    }

    settings_list <- readRDS("saved_setting/settings.rds")
    settings_abbr <- settings_list$indicator_abbr
    favorable_indicators <- settings_list$favorable_indicators

    custom_scales <- sapply(input$custom_indicators, function(indicator) {
      input[[paste0("scale_", indicator)]]
    }, simplify = FALSE)
    settings_list$custom_scales <- custom_scales

    withProgress(message = "Fetching data...", value = 0, {
      incProgress(0.2, detail = "Fetching population data for Region...")
      population_data_region <- fetch_population_data(input$org_units, strsplit(input$periods, ",")[[1]])

      incProgress(0.4, detail = "Fetching analytics data for Region...")
      analytics_data_region <- fetch_indicator_data(input$indicators, input$org_units, strsplit(input$periods, ",")[[1]])
      formatted_data_region <- format_analytics_data(analytics_data_region, indicators_metadata, org_units_metadata, population_data_region, indicator_map, dimension = "Region", settings_abbr = settings_abbr, custom_scales = custom_scales)

      if (length(input$zones) > 0) {
        incProgress(0.6, detail = "Fetching population data for Zone...")
        population_data_zone <- fetch_population_data(NULL, strsplit(input$periods, ",")[[1]], zone_ids = input$zones)

        incProgress(0.8, detail = "Fetching analytics data for Zone...")
        analytics_data_zone <- fetch_indicator_data(input$indicators, input$zones, strsplit(input$periods, ",")[[1]])
        formatted_data_zone <- format_analytics_data(analytics_data_zone, indicators_metadata, org_units_metadata, population_data_zone, indicator_map, dimension = "Zone", settings_abbr = settings_abbr, custom_scales = custom_scales)
      } else {
        formatted_data_zone <- data.frame()
      }

      if (length(input$woredas) > 0) {
        incProgress(0.9, detail = "Fetching population data for Woreda...")
        population_data_woreda <- fetch_population_data(NULL, strsplit(input$periods, ",")[[1]], woreda_ids = input$woredas)

        incProgress(1, detail = "Fetching analytics data for Woreda...")
        analytics_data_woreda <- fetch_indicator_data(input$indicators, input$woredas, strsplit(input$periods, ",")[[1]])
        formatted_data_woreda <- format_analytics_data(analytics_data_woreda, indicators_metadata, org_units_metadata, population_data_woreda, indicator_map, dimension = "Woreda", settings_abbr = settings_abbr, custom_scales = custom_scales)
      } else {
        formatted_data_woreda <- data.frame()
      }

      # Add Facility Type processing
      if (length(input$facility_types) > 0) {
        incProgress(0.85, detail = "Fetching Facility Type data...")

        analytics_data_facility <- fetch_indicator_data(
          input$indicators,
          input$org_units, # Parent org units for facility types
          strsplit(input$periods, ",")[[1]],
          facility_type_ids = input$facility_types
        )

        formatted_data_facility <- format_analytics_data(
          analytics_data_facility,
          indicators_metadata,
          org_units_metadata,
          population_data_region, # Or appropriate population data
          indicator_map,
          dimension = "Facility Type"
        )
      } else {
        formatted_data_facility <- data.frame()
      }

      # Add Settlement Type processing
      if (length(input$settlement_types) > 0) {
        incProgress(0.85, detail = "Fetching Settlement Type data...")

        analytics_data_settlement <- fetch_indicator_data(
          input$indicators,
          input$org_units, # Parent org units for settlement types
          strsplit(input$periods, ",")[[1]],
          settlement_type_ids = input$settlement_types
        )

        formatted_data_settlement <- format_analytics_data(
          analytics_data_settlement,
          indicators_metadata,
          org_units_metadata,
          population_data_region, # Or appropriate population data
          indicator_map,
          dimension = "Settlement"
        )
      } else {
        formatted_data_settlement <- data.frame()
      }

      # Combine all data
      data$combined <- rbind(
        formatted_data_region, formatted_data_zone,
        formatted_data_woreda, formatted_data_facility,
        formatted_data_settlement
      )

      # Convert columns to correct types
      data$combined[] <- lapply(data$combined, function(x) {
        if (is.list(x)) {
          return(unlist(x))
        } else {
          return(x)
        }
      })

      # Update favourable_indicator based on current input
      data$combined$favourable_indicator <- ifelse(
        data$combined$indicator_name %in% input$favorable_indicators,
        1,
        0
      )

      # Save the processed data (including favourable_indicator)
      saveRDS(data$combined, "fetched_data/main.rds")

      output$data_preview <- renderDT({
        datatable(data$combined, options = list(
          pageLength = 5, # 15
          lengthMenu = c(5, 10, 15, 20, 50, 100),
          scrollX = TRUE,
          autoWidth = TRUE,
          searching = TRUE,
          ordering = TRUE
        ))
      })
      data_fetched(TRUE)
      sendSweetAlert(session, title = "Success", text = "Data fetched successfully.", type = "success")
    })
  })


  # Dynamic subgroup filtering based on dimension selection
  observeEvent(input$filter_dimensions, {
    req(data$combined)

    if (length(input$filter_dimensions) > 0) {
      # Get unique subgroups for selected dimensions
      subgroups <- data$combined %>%
        filter(dimension %in% input$filter_dimensions) %>%
        pull(subgroup) %>%
        unique()
    } else {
      subgroups <- character(0)
    }

    updateSelectizeInput(session, "filter_subgroups",
      choices = subgroups,
      selected = input$filter_subgroups
    )
  })





  # Add at the beginning of server.R (with other reactive values)
  drilldown_data <- reactiveValues(
    org_hierarchy = NULL,
    regions = NULL,
    zones = NULL,
    woredas = NULL
  )

  # Load organization hierarchy data - with error handling
  observe({
    tryCatch(
      {
        org_units_json <- jsonlite::fromJSON("./meta/AIO_organisationUnits.json", simplifyVector = FALSE)
        if (!is.null(org_units_json$organisationUnits)) {
          drilldown_data$org_hierarchy <- org_units_json$organisationUnits

          # Extract regions, zones, and woredas
          regions <- unique(unlist(lapply(drilldown_data$org_hierarchy, function(x) {
            if (!is.null(x$parent$parent$displayName)) x$parent$parent$displayName
          })))
          zones <- unique(unlist(lapply(drilldown_data$org_hierarchy, function(x) {
            if (!is.null(x$parent$displayName)) x$parent$displayName
          })))
          woredas <- unique(unlist(lapply(drilldown_data$org_hierarchy, function(x) x$displayName)))

          drilldown_data$regions <- regions[!sapply(regions, is.null)]
          drilldown_data$zones <- zones[!sapply(zones, is.null)]
          drilldown_data$woredas <- woredas[!sapply(woredas, is.null)]

          # Update region filter
          updateSelectizeInput(session, "filter_regions", choices = drilldown_data$regions, server = TRUE)
        }
      },
      error = function(e) {
        showNotification(paste("Error loading organization units:", e$message), type = "error")
      }
    )
  })

  # When regions are selected, update zones in the zone filter
  observeEvent(input$filter_regions, {
    req(input$filter_regions)
    zones <- unique(unlist(lapply(drilldown_data$org_hierarchy, function(x) {
      if (x$parent$parent$displayName %in% input$filter_regions) x$parent$displayName
    })))
    zones <- zones[!sapply(zones, is.null)]
    updateSelectizeInput(session, "filter_zones_woreda", choices = zones, server = TRUE)
  })

  # When zones are selected, update woredas in the subgroup filter
  observeEvent(input$filter_zones_woreda, {
    req(input$filter_zones_woreda)
    woredas <- unique(unlist(lapply(drilldown_data$org_hierarchy, function(x) {
      if (x$parent$displayName %in% input$filter_zones_woreda) x$displayName
    })))
    woredas <- woredas[!sapply(woredas, is.null)]
    if ("Woreda" %in% input$filter_dimensions) {
      updateSelectizeInput(session, "filter_subgroups", choices = woredas, server = TRUE)
    }
  })

  observeEvent(input$filter_dimensions, {
    req(data$combined, input$filter_dimensions)

    # Get base subgroups based on selected dimensions
    subgroups <- data$combined %>%
      filter(dimension %in% input$filter_dimensions) %>%
      pull(subgroup) %>%
      unique()

    # Apply parent filters if applicable
    if (any(c("Zone", "Woreda") %in% input$filter_dimensions)) {
      if ("Zone" %in% input$filter_dimensions && !is.null(input$filter_regions)) {
        # Filter zones by selected regions
        zones_in_regions <- unique(unlist(lapply(drilldown_data$org_hierarchy, function(x) {
          if (!is.null(x$parent$parent) && x$parent$parent$displayName %in% input$filter_regions) {
            x$parent$displayName
          } else {
            NULL
          }
        })))
        zones_in_regions <- zones_in_regions[!sapply(zones_in_regions, is.null)]
        subgroups <- intersect(subgroups, zones_in_regions)
      }

      if ("Woreda" %in% input$filter_dimensions && !is.null(input$filter_zones_woreda)) {
        # Filter woredas by selected zones
        woredas_in_zones <- unique(unlist(lapply(drilldown_data$org_hierarchy, function(x) {
          if (!is.null(x$parent) && x$parent$displayName %in% input$filter_zones_woreda) {
            x$displayName
          } else {
            NULL
          }
        })))
        woredas_in_zones <- woredas_in_zones[!sapply(woredas_in_zones, is.null)]
        subgroups <- intersect(subgroups, woredas_in_zones)
      }
    }

    updateSelectizeInput(session, "filter_subgroups", choices = subgroups, server = TRUE)
  })


  ####################

  # Modify the apply_filters observer to include drill-down filters
  observeEvent(input$apply_filters, {
    req(data$combined)

    filtered_data <- data$combined

    # Apply basic filters
    if (!is.null(input$filter_indicators) && length(input$filter_indicators) > 0) {
      filtered_data <- filtered_data %>% filter(indicator_name %in% input$filter_indicators)
    }

    if (!is.null(input$filter_dimensions) && length(input$filter_dimensions) > 0) {
      filtered_data <- filtered_data %>% filter(dimension %in% input$filter_dimensions)
    }

    if (!is.null(input$filter_subgroups) && length(input$filter_subgroups) > 0) {
      filtered_data <- filtered_data %>% filter(subgroup %in% input$filter_subgroups)
    }

    if (!is.null(input$filter_dates) && length(input$filter_dates) > 0) {
      filtered_data <- filtered_data %>% filter(date %in% input$filter_dates)
    }

    data$filtered <- filtered_data

    # Update the data preview table
    output$data_preview <- renderDT({
      datatable(data$filtered, options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        searching = TRUE,
        ordering = TRUE
      ))
    })

    # Update plots
    output$distPlot <- renderPlotly({
      req(data$filtered)

      plot_ly(
        data = data$filtered, x = ~date, y = ~estimate, type = "scatter", mode = "markers",
        marker = list(color = input$plot_color, size = 5),
        text = ~ paste("Indicator:", indicator_name, "<br>Dimension:", dimension, "<br>Subgroup:", subgroup, "<br>Date:", date, "<br>Estimate:", estimate),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(
            # tickformat = "%Y", # Format as year only (no month/day)
            tickmode = "linear",
            dtick = 1, # step size of 1 year
            tickformat = ".0f" # no decimals
          )
        )
    })
  })


  # Update plots based on fetched data
  observe({
    req(data_fetched())

    output$distPlot <- renderPlotly({
      req(data$combined)
      plot_ly(
        data = data$combined, x = ~date, y = ~estimate, type = "scatter", mode = "markers",
        marker = list(color = input$plot_color, size = 3), # Use selected color
        text = ~ paste("Indicator:", indicator_name, "<br>Dimension:", dimension, "<br>Subgroup:", subgroup, "<br>Date:", date, "<br>Estimate:", estimate),
        hoverinfo = "text"
      )
    })

    output$bluePlot <- renderPlot({
      req(data$combined)
      x <- data$combined$estimate
      bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "steelblue", border = "white")
    })
  })

  # Export data to Excel and trigger download
  output$export_data <- downloadHandler(
    filename = function() {
      file_name <- paste("DHIS2_DATA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
      exported_file_name(file_name)
      file_name
    },
    content = function(file) {
      if (!is.null(data$combined) && nrow(data$combined) > 0) {
        withProgress(message = "Exporting data...", value = 0, {
          incProgress(0.5, detail = "Writing data to Excel...")
          write.xlsx(data$combined,
            file = file,
            sheetName = "Indicators_Data", rowNames = FALSE
          )
          incProgress(1, detail = "Export complete.")
          sendSweetAlert(session, title = "Success", text = paste("Excel file '", exported_file_name(), "' exported successfully."), type = "success")
        })
      } else {
        showModal(modalDialog(
          # title = "Error",
          # "Please Load Settings and Fetch Data Berfore Exporting!",
          sendSweetAlert(session, title = "Error", text = "Please Load Settings and Fetch Data Berfore Exporting to Excel!", type = "error"),
          # easyClose = TRUE,
          # footer = NULL
          return()
        ))
      }
    }
  )

  # Export data to Parquet and trigger download
  output$export_parquet <- downloadHandler(
    filename = function() {
      file_name <- paste("DHIS2_DATA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".parquet", sep = "")
      exported_file_name(file_name)
      file_name
    },
    content = function(file) {
      if (!is.null(data$combined) && nrow(data$combined) > 0) {
        withProgress(message = "Exporting data...", value = 0, {
          incProgress(0.5, detail = "Writing data to Parquet...")
          write_parquet(data$combined, file)
          incProgress(1, detail = "Export complete.")
          sendSweetAlert(session, title = "Success", text = paste("Parquet file '", exported_file_name(), "' exported successfully."), type = "success")
        })
      } else {
        showModal(modalDialog(
          # title = "Error",
          # "Please Load Settings and Fetch Data Berfore Exporting!",
          sendSweetAlert(session, title = "Error", text = "Please Load Settings and Fetch Data Berfore Exporting to Parquet!", type = "error"),
          # easyClose = TRUE,
          # footer = NULL
          return()
        ))
      }
    }
  )

  # Password protection
  #  showPasswordModal <- function(message = NULL) {
  #    showModal(modalDialog(
  #      title = "Authentication Required",
  #      passwordInput("auth_password", "Enter Password: [demo]"),
  #      if (!is.null(message)) div(style = "color: red;", message),
  #      footer = tagList(
  #        actionButton("auth_submit", "Submit")
  #      )
  #    ))
  #  }

  #  observe({
  #    showPasswordModal()
  #  })

  #  observeEvent(input$auth_submit, {
  #    if (input$auth_password == "mikeintosh") {
  #      user_role("mikeintosh")
  #      removeModal()
  #    } else if (input$auth_password == "demo") {
  #      user_role("demo")
  #      removeModal()
  #    } else {
  #      showPasswordModal("Incorrect password. Please try again.")
  #    }
  #  })

  #  output$role <- reactive({
  #    user_role()
  #  })
  #  outputOptions(output, "role", suspendWhenHidden = FALSE)

  # Show user guide
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "User Guide",
      HTML("
        <h3>Welcome to the DHIS2 Data Fetcher for HEAT Plus(+)</h3>
        <p>This application allows you to fetch and export data from DHIS2.</p>
        <h4>Steps to use the application:</h4>
        <ol>
          <li><b>Authentication:</b> Enter the correct password to access the application.</li>
          <li><b>DHIS2 Credentials:</b> Enter the DHIS2 Base URL, Username, and Password.</li>
          <li><b>Select Indicators:</b> Choose the indicators you want to fetch data for.</li>
          <li><b>Select Organisation Units:</b> Choose the organisation units you want to fetch data for.</li>
          <li><b>Select Zones and Woredas:</b> Choose the zones and woredas you want to fetch data for.</li>
          <li><b>Specify Periods:</b> Enter the periods (comma-separated) for which you want to fetch data.</li>
          <li><b>Save Settings:</b> Save your settings for future use. [For Super Users Only]</li>
          <li><b>Close This HELP Dialog:</b> Tap anywhere out of this HELP Dialog Box!</li>
      "),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Render dynamic plot based on selected view and chart type
  output$dynamicPlotOutput <- renderPlotly({
    req(data$filtered)
    print(names(data$filtered)) # Debug: Print column names
    plot_data <- data$filtered
    plot_type <- input$chart_type

    # Common plot parameters
    marker_params <- list(
      size = input$marker_size,
      color = input$plot_color
    )

    # Add this conditional line parameter handling
    line_params <- if (input$plot_mode %in% c("lines", "lines+markers")) {
      list(
        color = input$line_color,
        width = 2
      )
    } else {
      NULL
    }

    if (input$view_by == "Date") {
      if (plot_type == "Scatter") {
        p <- plot_ly(
          data = plot_data, x = ~date, y = ~estimate,
          type = "scatter", mode = input$plot_mode,
          marker = marker_params,
          line = line_params,
          text = ~ paste(
            "Indicator:", indicator_name, "<br>Dimension:",
            dimension, "<br>Subgroup:", subgroup,
            "<br>Date:", date, "<br>Estimate:", estimate
          ),
          hoverinfo = "text"
        )
      } else if (plot_type == "Bar") {
        p <- plot_ly(
          data = plot_data, x = ~date, y = ~estimate, type = "bar",
          marker = list(color = input$plot_color),
          text = ~ paste(
            "Indicator:", indicator_name, "<br>Dimension:",
            dimension, "<br>Subgroup:", subgroup,
            "<br>Date:", date, "<br>Estimate:", estimate
          ),
          hoverinfo = "text"
        )
      }
    } else if (input$view_by == "Dimension") {
      color_var <- ~dimension
      # Similar structure for Subgroup view
      if (plot_type == "Scatter") {
        p <- plot_ly(
          data = plot_data, x = ~dimension, y = ~estimate,
          type = "scatter", mode = input$plot_mode,
          marker = marker_params,
          line = line_params,
          text = ~ paste(
            "Indicator:", indicator_name, "<br>Dimension:",
            dimension, "<br>Subgroup:", subgroup,
            "<br>Date:", date, "<br>Estimate:", estimate
          ),
          hoverinfo = "text"
        )
      } else if (plot_type == "Bar") {
        p <- plot_ly(
          data = plot_data, x = ~dimension, y = ~estimate, type = "bar",
          marker = list(color = input$plot_color),
          text = ~ paste(
            "Indicator:", indicator_name, "<br>Dimension:",
            dimension, "<br>Subgroup:", subgroup,
            "<br>Date:", date, "<br>Estimate:", estimate
          ),
          hoverinfo = "text"
        )
      }
    } else {
      color_var <- ~subgroup
      # Similar structure for Subgroup view
      if (plot_type == "Scatter") {
        p <- plot_ly(
          data = plot_data, x = ~subgroup, y = ~estimate,
          type = "scatter", mode = input$plot_mode,
          marker = marker_params,
          line = line_params,
          text = ~ paste(
            "Indicator:", indicator_name, "<br>Dimension:",
            dimension, "<br>Subgroup:", subgroup,
            "<br>Date:", date, "<br>Estimate:", estimate
          ),
          hoverinfo = "text"
        )
      } else if (plot_type == "Bar") {
        p <- plot_ly(
          data = plot_data, x = ~subgroup, y = ~estimate, type = "bar",
          marker = list(color = input$plot_color),
          text = ~ paste(
            "Indicator:", indicator_name, "<br>Dimension:",
            dimension, "<br>Subgroup:", subgroup,
            "<br>Date:", date, "<br>Estimate:", estimate
          ),
          hoverinfo = "text"
        )
      }
    }
    # Add GeoJSON loading and map token
    # eth_geojson <- rjson::fromJSON(file = "saved_setting/geo/ethiopia_regions_map_simple.json") This is working

    # eth_geojson <- rjson::fromJSON(file = "saved_setting/geo/ethiopia_regions_map_simple.json")
    # eth_geojson <- jsonlite::fromJSON(file = ("saved_setting/geo/ethiopia_regions_map_simple.json"), warn = F)
    # eth_geojson <- fromJSON(tet = "saved_setting/geo/ethiopia_regions_map_simple.json")
    # eth_geojson <- fromJSON(text = "saved_setting/geo/ethiopia_regions_map_simple.json")


    if (input$chart_type == "Geo Heatmap") {
      # Debug: Print column names and first few rows of data$filtered
      print(names(data$filtered))
      print(head(data$filtered))
      # Prepare geographic data
      geo_data <- data$filtered %>%
        group_by(subgroup) %>%
        summarise(estimate = mean(estimate, na.rm = TRUE))

      # Create choropleth map
      p <- plot_ly(
        type = "choroplethmapbox",
        geojson = eth_geojson,
        featureidkey = "properties.shapeName",
        locations = geo_data$subgroup,
        z = geo_data$estimate,
        colorscale = "Viridis",
        marker = list(opacity = 0.7)
      ) %>%
        layout(
          mapbox = list(
            style = "light",
            zoom = 4.5,
            center = list(lon = 39.6, lat = 8.6), # Ethiopia coordinates
            accesstoken = mapboxToken
          ),
          margin = list(t = 0, b = 0, l = 0, r = 0)
        )

      return(p)
    }

    p %>% layout(showlegend = TRUE) # TRUE to show legend FALSE to hide legend
  })


  # Fetch metadata and populate select inputs
  observe({
    # Fetch Zones and Woredas metadata
    # zones_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=3&paging=false")$organisationUnits
    # woredas_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=4&paging=true&pageSize=600")$organisationUnits


    choices$indicators <- setNames(indicators_metadata$id, indicators_metadata$displayName)
    choices$org_units <- setNames(specific_org_units, org_units_metadata$displayName[org_units_metadata$id %in% specific_org_units])
    choices$zones <- setNames(zones_metadata$id, zones_metadata$displayName)
    choices$woredas <- setNames(woredas_metadata$id, woredas_metadata$displayName)

    updateSelectizeInput(session, "indicators", choices = choices$indicators, server = TRUE)
    updateSelectizeInput(session, "org_units", choices = choices$org_units, server = TRUE)
    updateSelectizeInput(session, "zones", choices = choices$zones, server = TRUE)
    updateSelectizeInput(session, "woredas", choices = choices$woredas, server = TRUE)
  })

  # Update Favorable Indicators based on selected indicators
  observe({
    if (!is.null(input$indicators)) {
      # Get the display names of the selected indicators
      favorable_choices <- indicators_metadata$displayName[indicators_metadata$id %in% input$indicators]
      updateSelectizeInput(session, "favorable_indicators", choices = favorable_choices, server = TRUE)
    }
  })

  # Select All functionality
  observeEvent(input$select_all_indicators, {
    if (input$select_all_indicators) {
      updateSelectizeInput(session, "indicators", selected = names(choices$indicators))
    } else {
      updateSelectizeInput(session, "indicators", selected = NULL)
    }
  })

  observeEvent(input$select_all_org_units, {
    if (input$select_all_org_units) {
      updateSelectizeInput(session, "org_units", selected = names(choices$org_units))
    } else {
      updateSelectizeInput(session, "org_units", selected = NULL)
    }
  })

  # Working For select all zones
  observeEvent(input$select_all_zones,
    {
      if (input$select_all_zones) {
        # Set credentials from inputs
        Sys.setenv(
          DHIS2_BASE_URL = input$base_url,
          DHIS2_USERNAME = input$username,
          DHIS2_PASSWORD = input$password
        )

        # Fetch zones from API
        zones_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=3&paging=false")$organisationUnits
        zone_choices <- setNames(zones_metadata$id, zones_metadata$displayName)

        # Update select input
        updateSelectizeInput(
          session,
          "zones",
          choices = zone_choices,
          selected = zone_choices
        )
      } else {
        updateSelectizeInput(session, "zones", selected = NULL)
      }
    },
    ignoreInit = TRUE
  )

  # Not working for select all zones
  #    observeEvent(input$select_all_zones, {
  #        if (input$select_all_zones) {
  #            updateSelectizeInput(session, "zones", selected = names(choices$zones))
  #        } else {
  #            updateSelectizeInput(session, "zones", selected = NULL)
  #        }
  #    })


  # Working For select all woredas
  observeEvent(input$select_all_woredas,
    {
      if (input$select_all_woredas) {
        # Set credentials from inputs
        Sys.setenv(
          DHIS2_BASE_URL = input$base_url,
          DHIS2_USERNAME = input$username,
          DHIS2_PASSWORD = input$password
        )

        # Fetch woredas from API
        woredas_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=4&paging=true&pageSize=600")$organisationUnits
        woreda_choices <- setNames(woredas_metadata$id, woredas_metadata$displayName)

        # Update select input
        updateSelectizeInput(
          session,
          "woredas",
          choices = woreda_choices,
          selected = woreda_choices
        )
      } else {
        updateSelectizeInput(session, "woredas", selected = NULL)
      }
    },
    ignoreInit = TRUE
  )

  # Not working for select all woredas
  #   observeEvent(input$select_all_woredas, {
  #       if (input$select_all_woredas) {
  #           updateSelectizeInput(session, "woredas", selected = names(choices$woredas))
  #       } else {
  #           updateSelectizeInput(session, "woredas", selected = NULL)
  #       }
  #   })

  # Add to existing select all observers
  observeEvent(input$select_all_facilities, {
    if (input$select_all_facilities) {
      updateSelectizeInput(session, "facility_types",
        selected = c(
          "kwcNbI9fPdB", "j8SCxUTyzfm",
          "FW4oru60vgc", "nVEDFMfnStv"
        )
      )
    } else {
      updateSelectizeInput(session, "facility_types", selected = NULL)
    }
  })


  observeEvent(input$select_all_settlements, {
    if (input$select_all_settlements) {
      updateSelectizeInput(session, "settlement_types",
        selected = c(
          "nKT0uoFbxdf", "ZktuKijP5jN", "V9sleOboZJ1"
        )
      )
    } else {
      updateSelectizeInput(session, "settlement_types", selected = NULL)
    }
  })

  # Save settings
  observeEvent(input$save_settings, {
    if (length(input$indicators) != length(strsplit(input$indicator_abbr, ",")[[1]])) {
      showModal(modalDialog(
        title = "Error",
        "The number of indicators and abbreviations must be equal.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    # Convert favorable indicator display names back to IDs
    favorable_indicator_ids <- indicators_metadata$id[indicators_metadata$displayName %in% input$favorable_indicators]

    custom_scales <- sapply(input$custom_indicators, function(indicator) {
      input[[paste0("scale_", indicator)]]
    }, simplify = FALSE)

    settings_list <- list(
      base_url = input$base_url,
      username = input$username,
      password = input$password,
      indicators = input$indicators,
      indicator_abbr = setNames(strsplit(input$indicator_abbr, ",")[[1]], input$indicators),
      favorable_indicators = favorable_indicator_ids, # Save favorable indicator IDs
      org_units = input$org_units,
      zones = input$zones,
      woredas = input$woredas,
      facility_types = input$facility_types,
      settlement_types = input$settlement_types, # Add settlement types
      periods = strsplit(input$periods, ",")[[1]],
      custom_scales = custom_scales # Save custom scales
    )

    saveRDS(settings_list, file = "saved_setting/settings.rds")
    sendSweetAlert(session, title = "Success", text = "Settings saved to file.", type = "success")
  })

  observeEvent(input$load_settings, {
    if (file.exists("saved_setting/settings.rds")) {
      settings_list <- readRDS("saved_setting/settings.rds")
      print(settings_list) # Print loaded settings to debug

      # Convert favorable indicator IDs back to display names
      favorable_indicator_names <- indicators_metadata$displayName[indicators_metadata$id %in% settings_list$favorable_indicators]

      updateTextInput(session, "base_url", value = settings_list$base_url)
      updateTextInput(session, "username", value = settings_list$username)
      updateTextInput(session, "password", value = settings_list$password)
      updateSelectizeInput(session, "indicators", selected = settings_list$indicators)
      updateTextInput(session, "indicator_abbr", value = paste(settings_list$indicator_abbr, collapse = ","))
      updateSelectizeInput(session, "favorable_indicators", selected = favorable_indicator_names) # Load favorable indicator display names
      updateSelectizeInput(session, "org_units", selected = settings_list$org_units)
      updateSelectizeInput(session, "zones", selected = settings_list$zones)
      updateSelectizeInput(session, "woredas", selected = settings_list$woredas)
      updateSelectizeInput(session, "facility_types", selected = settings_list$facility_types)
      updateSelectizeInput(session, "settlement_types", selected = settings_list$settlement_types) # Load settlement types
      updateTextInput(session, "periods", value = paste(settings_list$periods, collapse = ","))

      # Load custom scales
      if (!is.null(settings_list$custom_scales)) {
        updateSelectizeInput(session, "custom_indicators", selected = names(settings_list$custom_scales))

        # Use shinyjs::delay to ensure inputs are rendered
        delay(5, { # 50 milliseconds delay - adjust as needed
          for (indicator_id in names(settings_list$custom_scales)) {
            scale_input_id <- paste0("scale_", indicator_id)
            updateNumericInput(session, scale_input_id, value = settings_list$custom_scales[[indicator_id]])
          }
        })
      }

      sendSweetAlert(session, title = "Success", text = "Settings are loaded!", type = "success")
    } else {
      sendSweetAlert(session, title = "Error", text = "No Settings found!", type = "error")
    }
  })

  # Save source settings
  observeEvent(input$save_source_settings, {
    # if (user_role() == "mikeintosh") {
    if (user$info$role == "admin") {
      source_settings <- list(
        setting = input$setting,
        source = input$source,
        iso3 = input$iso3
      )
      saveRDS(source_settings, file = "saved_setting/source_settings.rds")
      sendSweetAlert(session, title = "Success", text = "Source settings saved to file.", type = "success")
    } else {
      sendSweetAlert(session, title = "Error", text = "You do not have permission to save source settings.", type = "error")
    }
  })

  # Load source settings
  observeEvent(input$load_source_settings, {
    if (file.exists("saved_setting/source_settings.rds")) {
      source_settings <- readRDS("saved_setting/source_settings.rds")
      updateTextInput(session, "setting", value = source_settings$setting)
      updateTextInput(session, "source", value = source_settings$source)
      updateTextInput(session, "iso3", value = source_settings$iso3)
      sendSweetAlert(session, title = "Success", text = "Source settings loaded from file.", type = "success")
    } else {
      sendSweetAlert(session, title = "Error", text = "No source settings found!", type = "error")
    }
  })

  # Render the UI components from exclean.R
  # Source exclean.R and render its UI components
  output$exclean_ui <- renderUI({
    source("exclean.R", local = TRUE)$value
  })
  #  output$dash_exclean_ui <- renderUI({
  #    # source("exclean.R", local = TRUE)$value
  #    source("clean.R", local = TRUE)$value
  #  })



  # Data Cleaning functionality
  # Add these to your existing server.R code

  # Data Cleaning functionality
  observeEvent(input$apply_na, {
    req(data$combined)
    df <- data$combined
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
    data$combined <- df
  })

  observeEvent(input$apply_dupes, {
    req(data$combined)
    if (input$remove_dupes) {
      data$combined <- data$combined %>% distinct()
    }
  })

  output$col_select <- renderUI({
    req(data$combined)
    selectInput("col", "Select Column:", choices = names(data$combined))
  })

  observeEvent(input$apply_convert, {
    req(input$col, data$combined)
    df <- data$combined
    if (input$convert_type == "Numeric") {
      df[[input$col]] <- as.numeric(df[[input$col]])
    } else if (input$convert_type == "Character") {
      df[[input$col]] <- as.character(df[[input$col]])
    } else if (input$convert_type == "Factor") {
      df[[input$col]] <- as.factor(df[[input$col]])
    } else if (input$convert_type == "Date") {
      df[[input$col]] <- as.Date(df[[input$col]])
    }
    data$combined <- df
  })

  output$rename_ui <- renderUI({
    req(data$combined)
    div(
      class = "rename-container", # Add a class for horizontal alignment
      lapply(names(data$combined), function(col) {
        textInput(paste0("rename_", col), paste("Rename", col), col)
      })
    )
  })

  observeEvent(input$apply_rename, {
    req(data$combined)
    df <- data$combined
    new_names <- sapply(names(df), function(col) input[[paste0("rename_", col)]])
    names(df) <- new_names[!is.na(new_names)]
    data$combined <- df
  })

  observeEvent(input$detect_outliers, {
    req(data$combined)
    df <- data$combined
    num_cols <- df %>% select(where(is.numeric))
    outlier_df <- map_dfr(num_cols, function(col) {
      z_scores <- scale(col)
      df[abs(z_scores) > input$zscore_threshold, ]
    })
    output$outliers <- renderDT(datatable(outlier_df))
  })

  observeEvent(input$save_clean, {
    req(data$combined)
    saveRDS(data$combined, "fetched_data/main.rds")
    showNotification("Cleaned data saved successfully!", type = "message")
  })

  output$download_clean <- downloadHandler(
    filename = "cleaned_data.rds",
    content = function(file) {
      saveRDS(data$combined, file)
    }
  )

  output$clean_table <- renderDT({
    req(data$combined)
    datatable(data$combined, options = list(scrollX = TRUE))
  })

# Summary Measures Calculations
calculate_summary_measures <- function(data) {
  req(data)

  # Ensure data has required columns
  required_cols <- c("estimate", "population", "dimension", "subgroup", "favourable_indicator")
  if (!all(required_cols %in% names(data))) {
    stop("Data missing required columns for summary measures calculation")
  }

  # Calculate setting average (weighted by population)
  setting_avg <- weighted.mean(data$estimate, data$population, na.rm = TRUE)

  # Determine reference subgroup
  if (data$favourable_indicator[1] == 1) {
    best_subgroup <- data$subgroup[which.max(data$estimate)]
  } else {
    best_subgroup <- data$subgroup[which.min(data$estimate)]
  }

  # Number of subgroups
  n_subgroups <- length(unique(data$subgroup))

  # Calculate relative ranks for ordered measures
  if ("subgroup_order" %in% names(data) && all(!is.na(data$subgroup_order))) {
    # For ordered dimensions
    data <- data[order(data$subgroup_order), ]
    data$relative_rank <- cumsum(data$population / sum(data$population)) -
      (data$population / sum(data$population)) / 2
  }

  # Initialize results list
  results <- list()

  # Simple measures
  results$difference <- max(data$estimate) - min(data$estimate)
  results$ratio <- max(data$estimate) / min(data$estimate)

  # Ordered disproportionality measures
  if ("relative_rank" %in% names(data)) {
    # Absolute Concentration Index (ACI)
    results$aci <- sum(data$population * (2 * data$relative_rank - 1) * data$estimate) / sum(data$population)

    # Relative Concentration Index (RCI)
    results$rci <- results$aci / setting_avg * 100

    # Slope Index of Inequality (SII) - simplified version
    model <- lm(estimate ~ relative_rank, data = data, weights = population)
    results$sii <- coef(model)[2]

    # Relative Index of Inequality (RII)
    pred_values <- predict(model, newdata = data.frame(relative_rank = c(0, 1)))
    results$rii <- pred_values[2] / pred_values[1]
  }

  # Variance measures
  results$bgv <- sum(data$population * (data$estimate - setting_avg)^2) / sum(data$population)
  results$bgsd <- sqrt(results$bgv)
  results$cov <- results$bgsd / setting_avg * 100

  # Mean difference measures
  results$mdmu <- mean(abs(data$estimate - setting_avg))
  results$mdmw <- sum(data$population * abs(data$estimate - setting_avg)) / sum(data$population)

  best_estimate <- if (data$favourable_indicator[1] == 1) max(data$estimate) else min(data$estimate)
  results$mdbu <- mean(abs(data$estimate - best_estimate))
  results$mdbw <- sum(data$population * abs(data$estimate - best_estimate)) / sum(data$population)

  if ("reference_subgroup" %in% names(data) && any(data$reference_subgroup == 1)) {
    ref_estimate <- data$estimate[data$reference_subgroup == 1][1]
    results$mdru <- mean(abs(data$estimate - ref_estimate))
    results$mdrw <- sum(data$population * abs(data$estimate - ref_estimate)) / sum(data$population)
  }

  # Index of disparity
  results$idisu <- results$mdmu / setting_avg * 100
  results$idisw <- results$mdmw / setting_avg * 100

  # Disproportionality measures
  shares <- data$estimate / setting_avg
  results$ti <- sum(data$population * shares * log(shares)) / sum(data$population) * 1000
  results$mld <- sum(data$population * (-log(shares))) / sum(data$population) * 1000

  # Impact measures
  results$par <- best_estimate - setting_avg
  results$paf <- results$par / setting_avg * 100

  return(results)
}

# Update summary measures inputs
observe({
  req(data$combined)

  updateSelectInput(session, "sm_indicator",
    choices = unique(data$combined$indicator_name)
  )

  updateSelectInput(session, "sm_dimension",
    choices = unique(data$combined$dimension)
  )

  updateSelectInput(session, "sm_date",
    choices = unique(data$combined$date)
  )
})

# Update reference subgroup choices based on selected dimension
observeEvent(input$sm_dimension, {
  req(data$combined, input$sm_dimension)

  subgroups <- unique(data$combined$subgroup[data$combined$dimension == input$sm_dimension])
  updateSelectInput(session, "sm_reference", choices = subgroups)
})

# Reactive for filtered data based on summary measures selections
sm_data <- reactive({
  req(data$combined, input$sm_indicator, input$sm_dimension, input$sm_date)

  filtered <- data$combined %>%
    filter(
      indicator_name == input$sm_indicator,
      dimension == input$sm_dimension,
      date == input$sm_date
    )

  # If ordered dimension, ensure ordering is correct
  if ("subgroup_order" %in% names(filtered) && all(!is.na(filtered$subgroup_order))) {
    filtered <- filtered[order(filtered$subgroup_order), ]
  }

  return(filtered)
})

# Calculate summary measures when button is clicked
sm_results <- eventReactive(input$sm_calculate, {
  req(sm_data())
  calculate_summary_measures(sm_data())
})

# Render all the summary measures outputs
output$sm_difference <- renderPrint({
  req(sm_results())
  cat("Difference (D):", round(sm_results()$difference, 4), "\n")
  cat("Interpretation: Absolute difference between highest and lowest subgroups.")
})

output$sm_ratio <- renderPrint({
  req(sm_results())
  cat("Ratio (R):", round(sm_results()$ratio, 4), "\n")
  cat("Interpretation: Relative ratio between highest and lowest subgroups.")
})

output$sm_aci <- renderPrint({
  req(sm_results())
  if (!is.null(sm_results()$aci)) {
    cat("Absolute Concentration Index (ACI):", round(sm_results()$aci, 4), "\n")
    cat("Interpretation: Measures absolute inequality across ordered subgroups.")
  } else {
    cat("ACI requires ordered dimension with subgroup ordering.")
  }
})


# Ordered Disproportionality Measures
output$sm_rci <- renderPrint({
  req(sm_results())
  if (!is.null(sm_results()$rci)) {
    cat("Relative Concentration Index (RCI):", round(sm_results()$rci, 4), "\n")
    cat("Interpretation: Measures relative inequality across ordered subgroups.")
  } else {
    cat("RCI requires ordered dimension with subgroup ordering.")
  }
})

# Regression-Based Measures
output$sm_sii <- renderPrint({
  req(sm_results())
  if (!is.null(sm_results()$sii)) {
    cat("Slope Index of Inequality (SII):", round(sm_results()$sii, 4), "\n")
    cat("Interpretation: Absolute difference between most and least advantaged, considering all subgroups.")
  } else {
    cat("SII requires ordered dimension with subgroup ordering.")
  }
})

output$sm_rii <- renderPrint({
  req(sm_results())
  if (!is.null(sm_results()$rii)) {
    cat("Relative Index of Inequality (RII):", round(sm_results()$rii, 4), "\n")
    cat("Interpretation: Relative ratio between most and least advantaged, considering all subgroups.")
  } else {
    cat("RII requires ordered dimension with subgroup ordering.")
  }
})

# Variance Measures
output$sm_bgv <- renderPrint({
  req(sm_results())
  cat("Between-Group Variance (BGV):", round(sm_results()$bgv, 4), "\n")
  cat("Interpretation: Weighted average of squared differences from setting average.")
})

output$sm_bgsd <- renderPrint({
  req(sm_results())
  cat("Between-Group Standard Deviation (BGSD):", round(sm_results()$bgsd, 4), "\n")
  cat("Interpretation: Square root of BGV - in same units as indicator.")
})

output$sm_cov <- renderPrint({
  req(sm_results())
  cat("Coefficient of Variation (COV):", round(sm_results()$cov, 4), "\n")
  cat("Interpretation: BGSD as percentage of setting average.")
})

# Mean Difference Measures
output$sm_mdmu <- renderPrint({
  req(sm_results())
  cat("Mean Difference from Mean (Unweighted):", round(sm_results()$mdmu, 4), "\n")
  cat("Interpretation: Average absolute difference from setting average.")
})

output$sm_mdmw <- renderPrint({
  req(sm_results())
  cat("Mean Difference from Mean (Weighted):", round(sm_results()$mdmw, 4), "\n")
  cat("Interpretation: Population-weighted average absolute difference from setting average.")
})

output$sm_mdbu <- renderPrint({
  req(sm_results())
  cat("Mean Difference from Best (Unweighted):", round(sm_results()$mdbu, 4), "\n")
  cat("Interpretation: Average absolute difference from best performing subgroup.")
})

output$sm_mdbw <- renderPrint({
  req(sm_results())
  cat("Mean Difference from Best (Weighted):", round(sm_results()$mdbw, 4), "\n")
  cat("Interpretation: Population-weighted average absolute difference from best performing subgroup.")
})

output$sm_mdru <- renderPrint({
  req(sm_results())
  if (!is.null(sm_results()$mdru)) {
    cat("Mean Difference from Reference (Unweighted):", round(sm_results()$mdru, 4), "\n")
    cat("Interpretation: Average absolute difference from reference subgroup.")
  } else {
    cat("No reference subgroup specified.")
  }
})

output$sm_mdrw <- renderPrint({
  req(sm_results())
  if (!is.null(sm_results()$mdrw)) {
    cat("Mean Difference from Reference (Weighted):", round(sm_results()$mdrw, 4), "\n")
    cat("Interpretation: Population-weighted average absolute difference from reference subgroup.")
  } else {
    cat("No reference subgroup specified.")
  }
})

# Disproportionality Measures
output$sm_ti <- renderPrint({
  req(sm_results())
  cat("Theil Index (TI):", round(sm_results()$ti, 4), "\n")
  cat("Interpretation: Measures inequality using logarithms of shares (sensitive to top differences).")
})

output$sm_mld <- renderPrint({
  req(sm_results())
  cat("Mean Log Deviation (MLD):", round(sm_results()$mld, 4), "\n")
  cat("Interpretation: Measures inequality using logarithms of shares (sensitive to bottom differences).")
})

# Impact Measures
output$sm_paf <- renderPrint({
  req(sm_results())
  cat("Population Attributable Fraction (PAF):", round(sm_results()$paf, 4), "%\n")
  cat("Interpretation: Potential relative improvement if all subgroups reached reference level.")
})

output$sm_par <- renderPrint({
  req(sm_results())
  cat("Population Attributable Risk (PAR):", round(sm_results()$par, 4), "\n")
  cat("Interpretation: Potential absolute improvement if all subgroups reached reference level.")
})


# Continue with similar render functions for all other measures...
# (I've shown a few examples, you'll need to implement all of them similarly)
# Highcharter plots for summary measures
# Add this to your server.R
hc_theme_custom <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(
      backgroundColor = "transparent",
      style = list(
        fontFamily = "Roboto, sans-serif"
      )
    ),
    title = list(
      style = list(
        color = "#333333",
        fontWeight = "bold"
      )
    ),
    colors = c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9")
  )
)

# Plots for visualization - Highcharter version
output$sm_difference_plot <- renderHighchart({
  req(sm_data())

  hchart(sm_data(), "column", hcaes(x = subgroup, y = estimate)) %>%
    hc_title(text = "Subgroup Estimates") %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Estimate")) %>%
    hc_tooltip(pointFormat = "<b>Subgroup:</b> {point.subgroup}<br><b>Estimate:</b> {point.y:.2f}") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl()) # hc_add_theme(hc_theme_custom)  # replace with custom theme use
})

output$sm_ratio_plot <- renderHighchart({
  req(sm_data())

  plot_data <- sm_data()
  plot_data$ratio_to_min <- plot_data$estimate / min(plot_data$estimate)

  hchart(plot_data, "column", hcaes(x = subgroup, y = ratio_to_min)) %>%
    hc_title(text = "Ratio to Minimum Subgroup") %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Ratio")) %>%
    hc_tooltip(pointFormat = "<b>Subgroup:</b> {point.subgroup}<br><b>Ratio:</b> {point.y:.2f}") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())
})

# Ordered Disproportionality Plots
output$sm_aci_plot <- renderHighchart({
  req(sm_data(), sm_results())
  if ("relative_rank" %in% names(sm_data())) {
    model <- lm(estimate ~ relative_rank, weights = population, data = sm_data())
    trend_data <- data.frame(
      relative_rank = sm_data()$relative_rank,
      trend = fitted(model)
    )

    hc <- highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_title(text = "Absolute Concentration") %>%
      hc_xAxis(title = list(text = "Relative Rank")) %>%
      hc_yAxis(title = list(text = "Estimate")) %>%
      hc_add_series(
        data = sm_data(),
        type = "scatter",
        hcaes(x = relative_rank, y = estimate, name = subgroup),
        name = "Data",
        marker = list(radius = 4)
      ) %>%
      hc_add_series(
        data = trend_data,
        type = "line",
        hcaes(x = relative_rank, y = trend),
        name = "Trend",
        color = "#FF0000"
      ) %>%
      hc_tooltip(pointFormat = "<b>Subgroup:</b> {point.name}<br><b>Estimate:</b> {point.y:.2f}") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())

    hc
  }
})

output$sm_rci_plot <- renderHighchart({
  req(sm_data(), sm_results())
  if ("relative_rank" %in% names(sm_data())) {
    plot_data <- sm_data()
    plot_data$relative_estimate <- plot_data$estimate / weighted.mean(plot_data$estimate, plot_data$population)

    model <- lm(relative_estimate ~ relative_rank, weights = population, data = plot_data)
    trend_data <- data.frame(
      relative_rank = plot_data$relative_rank,
      trend = fitted(model)
    )

    hc <- highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_title(text = "Relative Concentration") %>%
      hc_xAxis(title = list(text = "Relative Rank")) %>%
      hc_yAxis(title = list(text = "Relative Estimate")) %>%
      hc_add_series(
        data = plot_data,
        type = "scatter",
        hcaes(x = relative_rank, y = relative_estimate, name = subgroup),
        name = "Data",
        marker = list(radius = 4)
      ) %>%
      hc_add_series(
        data = trend_data,
        type = "line",
        hcaes(x = relative_rank, y = trend),
        name = "Trend",
        color = "#FF0000"
      ) %>%
      hc_tooltip(pointFormat = "<b>Subgroup:</b> {point.name}<br><b>Relative Estimate:</b> {point.y:.2f}") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())

    hc
  }
})

# Regression-Based Plots
output$sm_sii_plot <- renderHighchart({
  req(sm_data(), sm_results())
  if ("relative_rank" %in% names(sm_data())) {
    model <- lm(estimate ~ relative_rank, weights = population, data = sm_data())
    trend_data <- data.frame(
      relative_rank = sm_data()$relative_rank,
      trend = fitted(model)
    )

    hc <- highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_title(text = "Slope Index of Inequality") %>%
      hc_xAxis(title = list(text = "Relative Rank")) %>%
      hc_yAxis(title = list(text = "Estimate")) %>%
      hc_add_series(
        data = sm_data(),
        type = "scatter",
        hcaes(x = relative_rank, y = estimate, name = subgroup),
        name = "Data",
        marker = list(radius = 4)
      ) %>%
      hc_add_series(
        data = trend_data,
        type = "line",
        hcaes(x = relative_rank, y = trend),
        name = "SII Trend",
        color = "#FF0000"
      ) %>%
      hc_tooltip(pointFormat = "<b>Subgroup:</b> {point.name}<br><b>Estimate:</b> {point.y:.2f}") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())

    hc
  }
})

output$sm_rii_plot <- renderHighchart({
  req(sm_data(), sm_results())
  if ("relative_rank" %in% names(sm_data())) {
    plot_data <- sm_data()
    plot_data$relative_estimate <- plot_data$estimate / weighted.mean(plot_data$estimate, plot_data$population)

    model <- lm(relative_estimate ~ relative_rank, weights = population, data = plot_data)
    trend_data <- data.frame(
      relative_rank = plot_data$relative_rank,
      trend = fitted(model)
    )

    hc <- highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_title(text = "Relative Index of Inequality") %>%
      hc_xAxis(title = list(text = "Relative Rank")) %>%
      hc_yAxis(title = list(text = "Relative Estimate")) %>%
      hc_add_series(
        data = plot_data,
        type = "scatter",
        hcaes(x = relative_rank, y = relative_estimate, name = subgroup),
        name = "Data",
        marker = list(radius = 4)
      ) %>%
      hc_add_series(
        data = trend_data,
        type = "line",
        hcaes(x = relative_rank, y = trend),
        name = "RII Trend",
        color = "#FF0000"
      ) %>%
      hc_tooltip(pointFormat = "<b>Subgroup:</b> {point.name}<br><b>Relative Estimate:</b> {point.y:.2f}") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl())

    hc
  }
})

# Variance Plots
output$sm_variance_plot <- renderHighchart({
  req(sm_data())

  avg <- weighted.mean(sm_data()$estimate, sm_data()$population)

  hc <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Subgroup Estimates with Setting Average") %>%
    hc_xAxis(categories = sm_data()$subgroup, title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Estimate")) %>%
    hc_add_series(
      data = sm_data()$estimate,
      name = "Estimate",
      showInLegend = FALSE
    ) %>%
    hc_add_series(
      data = rep(avg, length(sm_data()$subgroup)),
      type = "line",
      name = "Setting Average",
      color = "#FF0000",
      dashStyle = "Dash",
      marker = list(enabled = FALSE)
    ) %>%
    hc_tooltip(
      formatter = JS("function() {
        if (this.series.name === 'Estimate') {
          return '<b>Subgroup:</b> ' + this.point.category + '<br><b>Estimate:</b> ' + Highcharts.numberFormat(this.y, 2);
        } else {
          return '<b>Setting Average:</b> ' + Highcharts.numberFormat(this.y, 2);
        }
      }")
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())

  hc
})

# Mean Difference Plots
output$sm_meandiff_plot <- renderHighchart({
  req(sm_data())

  fav <- sm_data()$favourable_indicator[1] == 1
  ref_value <- if (fav) max(sm_data()$estimate) else min(sm_data()$estimate)
  avg <- weighted.mean(sm_data()$estimate, sm_data()$population)

  hc <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Subgroup Estimates with Reference Lines") %>%
    hc_xAxis(categories = sm_data()$subgroup, title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Estimate")) %>%
    hc_add_series(
      data = sm_data()$estimate,
      name = "Estimate",
      showInLegend = FALSE
    ) %>%
    hc_add_series(
      data = rep(ref_value, length(sm_data()$subgroup)),
      type = "line",
      name = if (fav) "Best Subgroup" else "Worst Subgroup",
      color = "#00FF00",
      dashStyle = "Dash",
      marker = list(enabled = FALSE)
    ) %>%
    hc_add_series(
      data = rep(avg, length(sm_data()$subgroup)),
      type = "line",
      name = "Setting Average",
      color = "#FF0000",
      dashStyle = "Dash",
      marker = list(enabled = FALSE)
    ) %>%
    hc_tooltip(
      formatter = JS("function() {
        if (this.series.name === 'Estimate') {
          return '<b>Subgroup:</b> ' + this.point.category + '<br><b>Estimate:</b> ' + Highcharts.numberFormat(this.y, 2);
        } else {
          return '<b>' + this.series.name + ':</b> ' + Highcharts.numberFormat(this.y, 2);
        }
      }")
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())

  hc
})

# Disproportionality Plots
output$sm_ti_plot <- renderHighchart({
  req(sm_data())

  plot_data <- sm_data()
  plot_data$share <- plot_data$estimate / weighted.mean(plot_data$estimate, plot_data$population)

  hc <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Subgroup Shares Relative to Setting Average") %>%
    hc_xAxis(categories = plot_data$subgroup, title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Share")) %>%
    hc_add_series(
      data = plot_data$share,
      name = "Share",
      showInLegend = FALSE
    ) %>%
    hc_add_series(
      data = rep(1, length(plot_data$subgroup)),
      type = "line",
      name = "Equal Share",
      color = "#FF0000",
      dashStyle = "Dash",
      marker = list(enabled = FALSE)
    ) %>%
    hc_tooltip(
      formatter = JS("function() {
        if (this.series.name === 'Share') {
          return '<b>Subgroup:</b> ' + this.point.category + '<br><b>Share:</b> ' + Highcharts.numberFormat(this.y, 2);
        } else {
          return '<b>Equal Share:</b> ' + Highcharts.numberFormat(this.y, 2);
        }
      }")
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())

  hc
})

output$sm_mld_plot <- renderHighchart({
  req(sm_data())

  plot_data <- sm_data()
  plot_data$log_share <- log(plot_data$estimate / weighted.mean(plot_data$estimate, plot_data$population))

  hc <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Logarithm of Subgroup Shares") %>%
    hc_xAxis(categories = plot_data$subgroup, title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Log Share")) %>%
    hc_add_series(
      data = plot_data$log_share,
      name = "Log Share",
      showInLegend = FALSE
    ) %>%
    hc_add_series(
      data = rep(0, length(plot_data$subgroup)),
      type = "line",
      name = "Equal Share",
      color = "#FF0000",
      dashStyle = "Dash",
      marker = list(enabled = FALSE)
    ) %>%
    hc_tooltip(
      formatter = JS("function() {
        if (this.series.name === 'Log Share') {
          return '<b>Subgroup:</b> ' + this.point.category + '<br><b>Log Share:</b> ' + Highcharts.numberFormat(this.y, 2);
        } else {
          return '<b>Equal Share:</b> ' + Highcharts.numberFormat(this.y, 2);
        }
      }")
    ) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())

  hc
})

# Impact Measure Plots
output$sm_paf_plot <- renderHighchart({
  req(sm_data(), sm_results())

  plot_data <- data.frame(
    Metric = c("Current Average", "Potential Average"),
    Value = c(
      weighted.mean(sm_data()$estimate, sm_data()$population),
      weighted.mean(sm_data()$estimate, sm_data()$population) + sm_results()$par
    )
  )

  hchart(plot_data, "column", hcaes(x = Metric, y = Value)) %>%
    hc_title(text = "Potential Improvement from Eliminating Inequality") %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Estimate")) %>%
    hc_tooltip(pointFormat = "<b>{point.Metric}</b><br><b>Value:</b> {point.y:.2f}") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())
})

output$sm_par_plot <- renderHighchart({
  req(sm_data(), sm_results())

  plot_data <- data.frame(
    Metric = c("Current Average", "Potential Average"),
    Value = c(
      weighted.mean(sm_data()$estimate, sm_data()$population),
      weighted.mean(sm_data()$estimate, sm_data()$population) * (1 + sm_results()$paf / 100)
    )
  )

  hchart(plot_data, "column", hcaes(x = Metric, y = Value)) %>%
    hc_title(text = "Potential Improvement from Eliminating Inequality") %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Estimate")) %>%
    hc_tooltip(pointFormat = "<b>{point.Metric}</b><br><b>Value:</b> {point.y:.2f}") %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl())
})




# Data table output
output$sm_data_table <- renderDT({
  req(sm_data())
  datatable(sm_data(), options = list(scrollX = TRUE))
})

# Ordered dimensions UI
output$ordered_dimensions_ui <- renderUI({
  req(input$ordered_indicators, data$combined)

  lapply(input$ordered_indicators, function(ind) {
    fluidRow(
      column(6, h5(ind)),
      column(3, selectInput(paste0("ordered_dim_", ind), "Ordered Dimension",
        choices = unique(data$combined$dimension[data$combined$indicator_name == ind])
      )),
      column(3, uiOutput(paste0("reference_sub_ui_", ind)))
    )
  })
})

# Dynamic reference subgroup selection
observe({
  req(data$combined)
  inds <- unique(data$combined$indicator_name)
  lapply(inds, function(ind) {
    output[[paste0("reference_sub_ui_", ind)]] <- renderUI({
      dim_input <- paste0("ordered_dim_", ind)
      req(input[[dim_input]])

      subgroups <- unique(data$combined$subgroup[data$combined$indicator_name == ind &
        data$combined$dimension == input[[dim_input]]])

      selectInput(paste0("reference_sub_", ind), "Reference Subgroup",
        choices = subgroups
      )
    })
  })
})

# Update ordered indicators dropdown
observe({
  updateSelectizeInput(session, "ordered_indicators",
    choices = unique(data$combined$indicator_name)
  )
})

# Save ordered settings
observeEvent(input$save_ordered_settings, {
  req(input$ordered_indicators)

  ordered_settings <- list()
  for (ind in input$ordered_indicators) {
    dim_input <- paste0("ordered_dim_", ind)
    ref_input <- paste0("reference_sub_", ind)

    ordered_settings[[ind]] <- list(
      dimension = input[[dim_input]],
      reference = input[[ref_input]]
    )
  }

  saveRDS(ordered_settings, "saved_setting/ordered_settings.rds")
  showNotification("Ordered dimension settings saved successfully.", type = "message")
})



  # Load country metadata
  source("load_countries.R")

  # Benchmarking reactive values
  benchmark_data <- reactiveValues(
    parquet_data = NULL,
    country_meta = NULL,
    settings = NULL,
    comparison_data = NULL,
    reference_data = NULL,
    available_indicators = NULL,
    available_dimensions = NULL,
    available_countries = NULL,
    available_regions = NULL,
    available_incomes = NULL
  )

  # Load Parquet data when app starts
  observe({
    parquet_path <- "./data/indicators_data/HEAT_who_indicators.parquet"

    if (!file.exists(parquet_path)) {
      showNotification("Parquet data file not found", type = "error")
      return(NULL)
    }

    tryCatch(
      {
        # Read data
        df <- read_parquet(parquet_path)

        # Type conversion
        df <- df %>%
          mutate(across(c(
            date, estimate, se, ci_lb, ci_ub, population,
            setting_average, indicator_scale, subgroup_order
          ), as.numeric))

        # Store the data
        benchmark_data$parquet_data <- df

        # Extract metadata - using the safe rename approach
        benchmark_data$country_meta <- df %>%
          select(setting, iso3, whoreg6, wbincome2024) %>%
          distinct() %>%
          rename_with(~ case_when(
            . == "whoreg6" ~ "whoreg6_name",
            . == "wbincome2024" ~ "wbincome_name",
            TRUE ~ .
          ))

        # Update available options
        if (!is.null(df)) {
          benchmark_data$available_indicators <- unique(df$indicator_name)
          benchmark_data$available_dimensions <- unique(df$dimension)
          benchmark_data$available_countries <- unique(df$setting)
          benchmark_data$available_regions <- unique(df$whoreg6)
          benchmark_data$available_incomes <- unique(df$wbincome2024)

          updateSelectizeInput(session, "income_filter",
            choices = benchmark_data$available_incomes,
            server = TRUE
          )
          updateSelectizeInput(session, "who_region_filter",
            choices = benchmark_data$available_regions,
            server = TRUE
          )
          updateSelectizeInput(session, "country_select",
            choices = benchmark_data$available_countries,
            server = TRUE
          )
        }
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # Update country selection based on filters
  observe({
    req(benchmark_data$country_meta)

    filtered <- benchmark_data$country_meta

    if (!is.null(input$income_filter) && length(input$income_filter) > 0) {
      filtered <- filtered %>%
        filter(wbincome_name %in% input$income_filter)
    }

    if (!is.null(input$who_region_filter) && length(input$who_region_filter) > 0) {
      filtered <- filtered %>%
        filter(whoreg6_name %in% input$who_region_filter)
    }

    updateSelectizeInput(session, "country_select",
      choices = unique(filtered$setting),
      server = TRUE
    )
  })

  # Update inputs based on benchmark setting
  observeEvent(input$benchmark_setting, {
    if (input$benchmark_setting == "Country") {
      # Update indicator choices from Parquet data
      updateSelectizeInput(session, "benchmark_indicator_country",
        choices = benchmark_data$available_indicators,
        server = TRUE
      )

      # Update dimension choices from Parquet data
      updateSelectizeInput(session, "benchmark_dimension_country",
        choices = benchmark_data$available_dimensions,
        server = TRUE
      )
    } else {
      # Update indicator choices from DHIS2 data
      updateSelectizeInput(session, "benchmark_indicator",
        choices = unique(data$combined$indicator_name),
        server = TRUE
      )

      # Update dimension choices for Ethiopia
      current_choices <- switch(input$benchmark_setting,
        "Region" = c("Region", "Facility Type", "Settlement"),
        "Zone" = "Zone",
        "Woreda" = "Woreda",
        c("Region", "Zone", "Woreda", "Facility Type", "Settlement")
      )

      updateSelectizeInput(session, "benchmark_dimension",
        choices = current_choices
      )
    }
  })

  # Update date inputs
  observe({
    if (input$benchmark_setting == "Country" && !is.null(benchmark_data$parquet_data)) {
      dates <- unique(benchmark_data$parquet_data$date)
      updateSelectizeInput(session, "benchmark_specific_date",
        choices = sort(dates, decreasing = TRUE)
      )
    } else {
      req(data$combined)
      dates <- unique(data$combined$date)
      updateSelectizeInput(session, "benchmark_specific_date",
        choices = sort(dates, decreasing = TRUE)
      )
    }
  })

  # Update subgroup choices
  observeEvent(list(input$benchmark_dimension, input$benchmark_dimension_country), {
    if (input$benchmark_setting == "Country") {
      req(benchmark_data$parquet_data, input$benchmark_dimension_country)

      subgroups <- benchmark_data$parquet_data %>%
        filter(dimension %in% input$benchmark_dimension_country) %>%
        pull(subgroup) %>%
        unique()

      updateSelectizeInput(session, "benchmark_subgroup_country",
        choices = subgroups,
        server = TRUE
      )
    } else {
      req(data$combined, input$benchmark_dimension)

      subgroups <- data$combined %>%
        filter(dimension == input$benchmark_dimension) %>%
        pull(subgroup) %>%
        unique()

      updateSelectizeInput(session, "benchmark_subgroup",
        choices = subgroups
      )
    }
  })

  # Main benchmarking logic - Fixed Version
  observeEvent(input$apply_benchmark, {
    tryCatch(
      {
        if (input$benchmark_setting == "Country") {
          req(
            input$country_select, benchmark_data$parquet_data,
            input$benchmark_indicator_country, input$benchmark_dimension_country
          )

          if (length(input$country_select) < 1) {
            stop("Please select at least 1 country for comparison")
          }

          # Get date range
          if (input$benchmark_date_type == "Multiple Dates") {
            # if (input$benchmark_date_type == "Single Date") {
            req(input$benchmark_specific_date)
            date_filter <- as.numeric(input$benchmark_specific_date)
          } else {
            req(input$start_year, input$end_year)
            date_filter <- input$start_year:input$end_year
          }

          #          req(input$benchmark_specific_date) # Always require benchmark_specific_date
          #          date_filter <- as.numeric(input$benchmark_specific_date)

          # Filter data with proper error handling
          comparison_data <- tryCatch(
            {
              benchmark_data$parquet_data %>%
                filter(
                  setting %in% input$country_select,
                  indicator_name %in% input$benchmark_indicator_country,
                  dimension %in% input$benchmark_dimension_country,
                  date %in% date_filter
                ) %>%
                left_join(benchmark_data$country_meta, by = c("setting", "iso3")) %>%
                group_by(setting, iso3, whoreg6_name, wbincome_name, indicator_name, dimension, subgroup) %>%
                summarise(
                  mean_estimate = ifelse(all(is.na(estimate)), NA, mean(estimate, na.rm = TRUE)),
                  min_estimate = ifelse(all(is.na(estimate)), NA, min(estimate, na.rm = TRUE)),
                  max_estimate = ifelse(all(is.na(estimate)), NA, max(estimate, na.rm = TRUE)),
                  mean_ci_lb = ifelse(all(is.na(ci_lb)), NA, mean(ci_lb, na.rm = TRUE)),
                  mean_ci_ub = ifelse(all(is.na(ci_ub)), NA, mean(ci_ub, na.rm = TRUE)),
                  mean_population = ifelse(all(is.na(population)), NA, mean(as.numeric(population), na.rm = TRUE)),
                  .groups = "drop"
                )
            },
            error = function(e) {
              showNotification(paste("Data processing error:", e$message), type = "error")
              return(NULL)
            }
          )

          if (is.null(comparison_data)) {
            return()
          }

          benchmark_data$comparison_data <- comparison_data

          # Set reference data if subgroup is selected
          if (!is.null(input$benchmark_subgroup_country) &&
            input$benchmark_subgroup_country != "") {
            benchmark_data$reference_data <- tryCatch(
              {
                req(benchmark_data$comparison_data)
                benchmark_data$comparison_data %>%
                  filter(subgroup %in% input$benchmark_subgroup_country)
              },
              error = function(e) {
                showNotification(paste("Reference data error:", e$message), type = "error")
                return(NULL)
              }
            )
          } else {
            benchmark_data$reference_data <- NULL
          }
        } else {
          # Existing Ethiopia regional benchmarking code
          req(
            data$combined, input$benchmark_indicator,
            input$benchmark_dimension, input$benchmark_subgroup
          )

          # Get date range
          if (input$benchmark_date_type == "Multiple Dates") {
            #        if (input$benchmark_date_type == "Single Date") {
            req(input$benchmark_specific_date)
            date_filter <- input$benchmark_specific_date
          } else {
            req(input$start_year, input$end_year)
            date_filter <- input$start_year:input$end_year
          }

          #          req(input$benchmark_specific_date)  # Always require benchmark_specific_date
          #          date_filter <- input$benchmark_specific_date

          # Filter data with error handling
          comparison_data <- tryCatch(
            {
              data$combined %>%
                filter(
                  indicator_name == input$benchmark_indicator,
                  dimension == input$benchmark_dimension,
                  date %in% date_filter
                )
            },
            error = function(e) {
              showNotification(paste("Data filtering error:", e$message), type = "error")
              return(NULL)
            }
          )

          if (is.null(comparison_data)) {
            return()
          }

          # Calculate averages if date range is selected
          if (input$benchmark_date_type == "Date Range") {
            comparison_data <- comparison_data %>%
              group_by(subgroup, dimension) %>%
              summarise(
                estimate = ifelse(all(is.na(estimate)), NA, mean(estimate, na.rm = TRUE)),
                population = ifelse(all(is.na(population)), NA, mean(as.numeric(population), na.rm = TRUE)),
                .groups = "drop"
              )
          }

          benchmark_data$comparison_data <- comparison_data %>%
            arrange(desc(estimate))

          # Get reference data
          benchmark_data$reference_data <- tryCatch(
            {
              req(benchmark_data$comparison_data)
              benchmark_data$comparison_data %>%
                filter(subgroup == input$benchmark_subgroup)
            },
            error = function(e) {
              showNotification(paste("Reference data error:", e$message), type = "error")
              return(NULL)
            }
          )

          # Calculate differences
          if (!is.null(benchmark_data$reference_data) && nrow(benchmark_data$reference_data) > 0) {
            ref_value <- mean(benchmark_data$reference_data$estimate)

            benchmark_data$comparison_data <- benchmark_data$comparison_data %>%
              mutate(
                is_benchmark = subgroup == input$benchmark_subgroup,
                difference = estimate - ref_value,
                pct_difference = (difference / ref_value) * 100
              )
          }
        }
      },
      error = function(e) {
        showNotification(paste("Benchmarking error:", e$message), type = "error")
      }
    )
  })

  # Fixed Benchmark visualization
  output$benchmark_plot <- renderPlotly({
    req(benchmark_data$comparison_data)

    if (input$benchmark_setting == "Country") {
      req(benchmark_data$comparison_data)
      plot_data <- benchmark_data$comparison_data

      # Verify required columns exist
      required_cols <- c("setting", "mean_estimate", "subgroup", "indicator_name", "dimension", "mean_population")
      if (!all(required_cols %in% names(plot_data))) {
        return(plotly_empty() %>% layout(title = "Required data columns not available"))
      }

      # Create different plot types
      p <- switch(input$benchmark_chart_type,
        "Bar" = {
          plot_ly(plot_data,
            x = ~setting, y = ~mean_estimate,
            type = "bar",
            color = ~subgroup,
            text = ~ paste(
              "Country:", setting,
              "<br>Indicator:", indicator_name,
              "<br>Dimension:", dimension,
              "<br>Subgroup:", subgroup,
              "<br>Estimate:", round(mean_estimate, 2),
              "<br>Population:", round(mean_population)
            ),
            hoverinfo = "text"
          ) %>%
            layout(
              barmode = input$benchmark_bar_mode,
              xaxis = list(title = "", categoryorder = "total descending"),
              yaxis = list(title = "Estimate"),
              showlegend = TRUE
            )
        },
        "Horizontal Bar" = {
          plot_ly(plot_data,
            y = ~setting, x = ~mean_estimate,
            type = "bar",
            orientation = "h",
            color = ~subgroup,
            text = ~ paste(
              "Country:", setting,
              "<br>Estimate:", round(mean_estimate, 2),
              "<br>Subgroup:", subgroup
            ),
            hoverinfo = "text"
          ) %>%
            layout(
              barmode = input$benchmark_bar_mode,
              yaxis = list(title = "", categoryorder = "total ascending"),
              xaxis = list(title = "Estimate"),
              showlegend = TRUE
            )
        },
        "Scatter" = {
          plot_ly(plot_data,
            x = ~setting, y = ~mean_estimate,
            type = "scatter",
            mode = "markers",
            color = ~subgroup,
            size = ~mean_population,
            text = ~ paste(
              "Country:", setting,
              "<br>Subgroup:", subgroup,
              "<br>Estimate:", round(mean_estimate, 2)
            ),
            hoverinfo = "text"
          ) %>%
            layout(
              xaxis = list(title = ""),
              yaxis = list(title = "Estimate"),
              showlegend = TRUE
            )
        },
        "Line" = {
          plot_ly(plot_data,
            x = ~setting, y = ~mean_estimate,
            type = "scatter",
            mode = "lines+markers",
            color = ~subgroup,
            text = ~ paste(
              "Country:", setting,
              "<br>Subgroup:", subgroup,
              "<br>Estimate:", round(mean_estimate, 2)
            ),
            hoverinfo = "text"
          ) %>%
            layout(
              xaxis = list(title = ""),
              yaxis = list(title = "Estimate"),
              showlegend = TRUE
            )
        }
      )

      # Add reference line if reference data exists
      if (!is.null(benchmark_data$reference_data) &&
        nrow(benchmark_data$reference_data) > 0 &&
        "mean_estimate" %in% names(benchmark_data$reference_data)) {
        ref_value <- mean(benchmark_data$reference_data$mean_estimate, na.rm = TRUE)
        if (!is.na(ref_value)) {
          p <- p %>% add_lines(
            x = ~ unique(setting),
            y = ref_value,
            line = list(color = "red", dash = "dot"),
            name = "Reference",
            showlegend = TRUE
          )
        }
      }

      return(p)
    } else {
      # Existing regional benchmarking plot code
      req(benchmark_data$reference_data)
      plot_data <- benchmark_data$comparison_data

      # Verify required columns exist
      required_cols <- c("subgroup", "estimate", "is_benchmark", "difference", "pct_difference", "population")
      if (!all(required_cols %in% names(plot_data))) {
        return(plotly_empty() %>% layout(title = "Required data columns not available"))
      }

      plot_ly(
        data = plot_data,
        x = ~ reorder(subgroup, estimate),
        y = ~estimate,
        type = "bar",
        color = ~is_benchmark,
        colors = c("#1f77b4", "#ff7f0e"),
        text = ~ paste(
          "Subgroup:", subgroup,
          "<br>Estimate:", round(estimate, 2),
          "<br>Difference:", round(difference, 2),
          "<br>% Difference:", round(pct_difference, 2), "%",
          "<br>Population:", round(population)
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          title = paste("Benchmark Comparison for", input$benchmark_indicator),
          xaxis = list(title = ""),
          yaxis = list(title = "Estimate"),
          showlegend = FALSE,
          shapes = if (nrow(benchmark_data$reference_data) > 0) {
            list(
              type = "line",
              x0 = -0.5,
              x1 = nrow(plot_data) - 0.5,
              y0 = benchmark_data$reference_data$estimate[1],
              y1 = benchmark_data$reference_data$estimate[1],
              line = list(color = "red", dash = "dot")
            )
          },
          annotations = if (nrow(benchmark_data$reference_data) > 0) {
            list(
              x = nrow(plot_data) - 1,
              y = benchmark_data$reference_data$estimate[1],
              text = paste("Benchmark:", input$benchmark_subgroup),
              showarrow = FALSE,
              xanchor = "right"
            )
          }
        )
    }
  })

  # Fixed Benchmark table output
  output$benchmark_table <- renderDT({
    req(benchmark_data$comparison_data)

    if (input$benchmark_setting == "Country") {
      # Verify columns exist before selecting
      required_cols <- c(
        "setting", "whoreg6_name", "wbincome_name", "indicator_name",
        "dimension", "subgroup", "mean_estimate", "min_estimate",
        "max_estimate", "mean_ci_lb", "mean_ci_ub", "mean_population"
      )

      if (!all(required_cols %in% names(benchmark_data$comparison_data))) {
        return(datatable(data.frame(Error = "Required columns not available")))
      }

      table_data <- benchmark_data$comparison_data %>%
        select(
          Country = setting,
          `WHO Region` = whoreg6_name,
          `Income Group` = wbincome_name,
          Indicator = indicator_name,
          Dimension = dimension,
          Subgroup = subgroup,
          `Mean Estimate` = mean_estimate,
          `Min Estimate` = min_estimate,
          `Max Estimate` = max_estimate,
          `CI Lower` = mean_ci_lb,
          `CI Upper` = mean_ci_ub,
          Population = mean_population
        )
    } else {
      # Verify columns exist before selecting
      required_cols <- c(
        "subgroup", "estimate", "difference", "pct_difference",
        "ci_lb", "ci_ub", "population"
      )

      if (!all(required_cols %in% names(benchmark_data$comparison_data))) {
        return(datatable(data.frame(Error = "Required columns not available")))
      }

      table_data <- benchmark_data$comparison_data %>%
        select(
          Subgroup = subgroup,
          Estimate = estimate,
          Difference = difference,
          `% Difference` = pct_difference,
          `CI Lower` = ci_lb,
          `CI Upper` = ci_ub,
          Population = population
        )
    }

    datatable(
      table_data,
      extensions = c("Buttons", "Responsive"),
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        responsive = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = sapply(table_data, is.numeric), digits = 2)
  })

  # Pivot table output
  output$pivot_table <- renderRpivotTable({
    req(benchmark_data$comparison_data)

    if (input$benchmark_setting == "Country") {
      req(benchmark_data$comparison_data)
      rpivotTable(benchmark_data$comparison_data,
        rows = c("setting", "dimension"),
        cols = c("indicator_name", "date"),
        vals = "mean_estimate",
        aggregatorName = "Average",
        rendererName = "Table"
      )
    } else {
      rpivotTable(benchmark_data$comparison_data,
        rows = c("subgroup", "dimension"),
        cols = c("indicator_name", "date"),
        vals = "estimate",
        aggregatorName = "Average",
        rendererName = "Table"
      )
    }
  })

  # Summary statistics output
  output$benchmark_summary <- renderPrint({
    req(benchmark_data$comparison_data)

    if (input$benchmark_setting == "Country") {
      cat("Country Comparison Summary Statistics\n")
      cat("=====================================\n\n")

      if (!is.null(benchmark_data$comparison_data)) {
        # Summary by country
        if ("setting" %in% names(benchmark_data$comparison_data) &&
          "mean_estimate" %in% names(benchmark_data$comparison_data)) {
          cat("By Country:\n")
          print(benchmark_data$comparison_data %>%
            group_by(setting) %>%
            summarise(
              Mean = mean(mean_estimate, na.rm = TRUE),
              Median = median(mean_estimate, na.rm = TRUE),
              SD = sd(mean_estimate, na.rm = TRUE),
              Min = min(mean_estimate, na.rm = TRUE),
              Max = max(mean_estimate, na.rm = TRUE),
              .groups = "drop"
            ))
        }

        # Summary by dimension
        if ("dimension" %in% names(benchmark_data$comparison_data)) {
          cat("\nBy Dimension:\n")
          print(benchmark_data$comparison_data %>%
            group_by(dimension) %>%
            summarise(
              Mean = mean(mean_estimate, na.rm = TRUE),
              Median = median(mean_estimate, na.rm = TRUE),
              SD = sd(mean_estimate, na.rm = TRUE),
              Min = min(mean_estimate, na.rm = TRUE),
              Max = max(mean_estimate, na.rm = TRUE),
              .groups = "drop"
            ))
        }

        # Summary by indicator
        if ("indicator_name" %in% names(benchmark_data$comparison_data)) {
          cat("\nBy Indicator:\n")
          print(benchmark_data$comparison_data %>%
            group_by(indicator_name) %>%
            summarise(
              Mean = mean(mean_estimate, na.rm = TRUE),
              Median = median(mean_estimate, na.rm = TRUE),
              SD = sd(mean_estimate, na.rm = TRUE),
              Min = min(mean_estimate, na.rm = TRUE),
              Max = max(mean_estimate, na.rm = TRUE),
              .groups = "drop"
            ))
        }
      }
    } else {
      # Existing regional summary code
      if ("estimate" %in% names(benchmark_data$comparison_data)) {
        summary_data <- benchmark_data$comparison_data$estimate
        cat("Regional Benchmark Summary:\n")
        cat("===========================\n")
        print(summary(summary_data))
        cat("\nStandard Deviation:", sd(summary_data, na.rm = TRUE))
        cat("\nMinimum:", min(summary_data, na.rm = TRUE))
        cat("\nMaximum:", max(summary_data, na.rm = TRUE))
        cat("\nNumber of Subgroups:", length(unique(benchmark_data$comparison_data$subgroup)))

        if (!is.null(benchmark_data$reference_data) &&
          "estimate" %in% names(benchmark_data$reference_data)) {
          cat("\n\nBenchmark Reference (", input$benchmark_subgroup, "):\n", sep = "")
          cat("Mean Estimate:", mean(benchmark_data$reference_data$estimate, na.rm = TRUE))
        }
      }
    }
  })

  # Download handler
  output$download_benchmark <- downloadHandler(
    filename = function() {
      paste("benchmark_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(benchmark_data$comparison_data, file, row.names = FALSE)
    }
  )



  # output$ethgeoUI <- renderUI({
  #  source("ethgeo.R", local = TRUE)$value
  # })


  source("spatial_module.R")
  spatialUI("spatial_module")

  # Near other source() calls at the top
  source("ethgeo.R")

  # In the server function, add:
  ethgeoUI("ethgeo_module")


  source("dba.R")
  dba_module_ui("dba_module")

  source("manual.R")
  manualServer("manual_module", reactive(user$info$role))
  # Add this to your server function in server.R
  manualUI("manual_module")

  # Add this with your other source() calls at the top of server.R
  # source("manual.R", local = TRUE)$value
  # source("manual.R")
}

# Run the application
# shinyApp(ui = ui, server = server)
