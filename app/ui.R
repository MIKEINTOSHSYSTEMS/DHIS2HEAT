library(shiny)
library(shinydashboard) # for Dashboard
library(shinyWidgets) # for radio button widgets
library(shinydashboardPlus)
library(shinyjs) # to perform common useful JavaScript operations in Shiny apps
library(shinyBS) # for bsTooltip function
library(shinyalert) # for alert message very nice format
library(plyr) # empty() function is from this package
library(dplyr) # select functions are covered in the require
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
library(arrow) # Add arrow library for Parquet support
library(rjson)
library(jsonlite)
library(slickR)
library(rpivotTable) # For pivot table functionality
library(purrr)
library(viridisLite)
library(sf)
library(highcharter)
library(DT) # for using %>% which works as a pipe in R code

# Source the UI and server components of data exploration and cleaning
# source("exclean.R")
# source("clean.R", local = TRUE)$value
source("load_countries.R", local = TRUE)$value
source("manual.R", local = TRUE)$value
# source("spatial.R", local = TRUE)$value
source("spatial_module.R", local = TRUE)$value
source("exclean.R", local = TRUE)$value
source("ethgeo.R", local = TRUE)$value
source("dba.R", local = TRUE)$value
# source("geoheatmap.R", local = TRUE)$value

# Custom CSS for hiding the sidebar and settings
css <- "

"

# Define specific organisation units (regions)
specific_org_units <- c(
  "yY9BLUUegel", "UFtGyqJMEZh", "yb9NKGA8uqt", "Fccw8uMlJHN",
  "tDoLtk2ylu4", "G9hDiPNoB7d", "moBiwh9h5Ce", "b9nYedsL8te",
  "XU2wpLlX4Vk", "xNUoZIrGKxQ", "PCKGSJoNHXi", "a2QIIR2UXcd",
  "HIlnt7Qj8do", "Gmw0DJLXGtx"
)

# Define UI
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      body { 
        font-family: 'Roboto', sans-serif;
        overflow-x: hidden;
      }
      .start-page {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
    "))
  ),
  div(
    id = "preloader",
    tags$a(
      img(src = "moh_logo_blue.svg", height = "100px"),
      href = "#",
      # style = "color: #007BDDFF; font-weight: bold; font-size: 16px; padding: 15px;",
      # " Ministry of Health - ETHIOPIA"
    ),
    hr(),
    div(
      class = "progress-container",
      div(id = "progress-bar", class = "progress-bar"),
      div(id = "progress-percent", class = "progress-percent", "0%") # Add this line
    ),
    div(
      class = "loader-text loading-pulse",
      # p("Loading Health Equity Assessment Dashboard..."),
      HTML("<span>Loading </span><span style='color: #007dc9; font-weight: bold;'>Health Equity Assessment </span><span>Dashboard...</span>"),
      hr(),
      tags$img(src = "dhis2-icon.svg", height = "50px"),
      HTML("<span style='color: #009BDDFF; font-weight: bold;'>DHIS2 </span><span>Data Fetcher for </span><span style='color: #007dc9; font-weight: bold;'>HEAT </span><span style='color: #b5e71c; font-weight: bold;'>Plus(+)</span>"),
    ),
    tags$script(HTML("
    // Simulate progress
    let progress = 0;
    const progressBar = document.getElementById('progress-bar');
    const progressPercent = document.getElementById('progress-percent'); // Add this line
    const startTime = Date.now();
    const minDuration = 47000; // Minimum duration in milliseconds (7900 = 7.9 sec) (45 seconds)
    const progressInterval = setInterval(() => {
      const elapsedTime = Date.now() - startTime;
      progress += Math.random() * 7; // Adjust the increment to control the speed
      progress = Math.min(progress, 100);
      progressBar.style.width = progress + '%';
      progressPercent.textContent = Math.floor(progress) + '%'; // Add this line

      if (progress >= 100 && elapsedTime >= minDuration) {
        clearInterval(progressInterval);
      }
    }, 200);

    // Hide preloader when Shiny is ready
    $(document).on('shiny:connected', function(event) {
      const elapsedTime = Date.now() - startTime;
      const remainingTime = Math.max(0, minDuration - elapsedTime);
      setTimeout(() => {
        clearInterval(progressInterval);
        progressBar.style.width = '100%';
        progressPercent.textContent = '100%'; // Add this line
        setTimeout(() => {
          $('#preloader').fadeOut(500, function() {
            $(this).remove();
          });
        }, 300);
      }, remainingTime);
    });
  "))
  ),
  shinyjs::useShinyjs(),
  navbarPage(
    # title = div(img(src = "dhis2-icon.svg", height = "30px"),
    #            "DHIS2 Data Fetcher"),
#    title = tags$div(
#      tags$img(src = "dhis2-icon.svg", height = "50px"),
#      HTML("<span style='color: #d7d7d7;'>DHIS2 Data Fetcher for </span><span style='color: #007dc9; font-weight: bold;'>HEAT </span><span style='color: #b5e71c; font-weight: bold;'>Plus(+)</span>")
#    ),
    id = "main_nav",
    windowTitle = "DHIS2 Data Fecher & Visualizations for HEAT+",
    # theme = shinytheme("flatly"),

    # Conditionally display "Home" tab for non-logged-in users
    # Home Tab
    conditionalPanel(
      condition = "output.logged_in == false",
      tabPanel("Home",
        value = "home",
        class = "home-tab",
        fluidPage(
          class = "start-page",
          # tags$a(
          tags$
            img(src = "moh_logo_blue.svg", height = "100px"),
          # href = "#",
          # style = "color: #007BDDFF; font-weight: bold; font-size: 16px; padding: 15px;",
          # " Ministry of Health - ETHIOPIA"
          # ),
          hr(),
          hr(),
          h1("Welcome To"),
          tags$img(src = "dhis2-icon.svg", height = "50px"),
          HTML("<span style='color: #009BDDFF; font-weight: bold;'>DHIS2 </span><span>Data Fetcher for </span><span style='color: #007dc9; font-weight: bold;'>HEAT </span><span style='color: #b5e71c; font-weight: bold;'>Plus(+)</span>"),
          hr(),
          p("Comprehensive data management and Health Equity Assessment and Analysis platform"),
          p("Use demo/demo for username and password to login and test the system"),
          p("You can also register and create your account to test the system"),
          hr(),
          hr(),
          actionButton("login_btn", "Login", class = "btn-primary"),
          actionButton("register_btn", "Register", class = "btn-success"),
          hr(),
          hr(),
          # Content Slider
          div(
            class = "section animated fadeIn",
            hr(),
            hr(),
            # h2("Features"),
            # slickROutput("content_slider", width = "100%")
            slickROutput("content_slider", width = "500px")
            # slickROutput("content_slider", width = "100%")
          ),

          # Release Notes
          div(
            class = "release-notes animated fadeIn",
            h2("Major Release Notes"),
            p("Version 1.0.0 - Initial release with core features."),
            p("Version 1.1.0 - Added data visualization and export options."),
            p("Version 1.1.1 - Added Reactive components and Dynamic Updates."),
            p("Version 1.2.0 - Improved user interface and performance."),
            p("Version 1.3.0 - Refactored UI: Updated logo links, enhanced navbar styling, and improved layout responsiveness."),
            p("Version 1.4.0 - Enhanced UI: Implemented sticky header with animations, optimized navbar and dropdown styles, and improved mobile responsiveness."),
            p("Version 1.5.0 - Refactored ethgeo module: Streamlined data processing, enhanced map visualization, and improved UI elements."),
            p("Version 1.5.1 - Refactored ethgeo module: Replaced Plotly with Highcharter for map visualization, enhanced data filtering, and improved UI layout."),
            p("Version 1.6.0 - Added spatial module with WHO Benchmarking Tool, interactive map, and comparison table."),
            p("Version 1.6.1 - Improved income group selection in spatial module and updated filtering logic."),
            p("Version 1.6.2 - Added spatial health indicators benchmarking tool with dynamic UI and map visualization."),
            p("Version 1.7.0 - Added benchmarking settings and enhanced spatial data management functionalities."),
            p("Version 1.7.1 - Modern visualizations added with WHO Benchmarking Standards, advanced data management functionalities, and streamlined UI."),
          ),

          # Additional Sections
          div(
            class = "section animated fadeIn",
            hr(),
            h2("More Information"),
            p("For more information, visit our website or contact support."),
            hr(),
            HTML("Version-1.7.1 | &copy; 2025 Designed & Developed by: <a href='https://merqconsultancy.org'><b>MERQ Consultancy</b>.</a>")
          )
        )
      )
    ),

    # Main Application (conditional display)
    conditionalPanel(
      condition = "output.logged_in",
      dashboardPage(
        dashboardHeader(
          title = tags$div(
            tags$img(src = "dhis2-icon.svg", height = "50px"),
            HTML("<span style='color: #d7d7d7;'>DHIS2 Data Fetcher for </span><span style='color: #007dc9; font-weight: bold;'>HEAT </span><span style='color: #b5e71c; font-weight: bold;'>Plus(+)</span>")
          ),
          tags$li(
            class = "dropdown",
            # tags$a(
            tags$html(
              img(src = "moh_logo_white.png", height = "47px", style = "padding-left: 7px", ),
              # href = "#",
              # style = "vertical-align: middle; margin-right: 10px;",
              style = "vertical-align: sub; margin-right: 10px;",
              HTML("<span style='vertical-align: sub;color: white;font-weight: bold;font-size: 12px;padding: 13px;'>Ministry of Health - ETHIOPIA | Health Equity Assessment Toolkit (HEAT)</span>"),

              # style = "color: white; font-weight: bold; font-size: 16px; padding: 15px;",
              # " Ministry of Health - ETHIOPIA"
            )
          ),
          titleWidth = "350px" # Match sidebar width
        ),
        dashboardSidebar(
          width = 350,
#          hr(),
#          tags$html(
#            useShinyjs(), # Activate shinyjs functionality
#            class = "pdropdown user-menu",
#            uiOutput("user_profile")
#          ),
############################
          #   tags$html(
          #     #class = "dropdown user-menu",
          #     class = "pdropdown user-menu",
          #   tags$head(tags$style(HTML(css))),
          #   tags$script(HTML("
          #     $(document).ready(function() {
          #       // Toggle dropdown menu when user clicks on the profile picture or username

          #      });
          #    ")),
          #      uiOutput("user_profile")
          #    ),
          hr(),
          sidebarMenu(
            menuItem("Data Preview", tabName = "data_preview", icon = icon("eye")),
            menuItem("Summary Measures", tabName = "summary_measures", icon = icon("calculator")),
            menuItem("Benchmarking", tabName = "benchmarking", icon = icon("chart-line")),
            menuItem("WHO GeoBenchmarking", tabName = "geographical", icon = icon("map-location")),
            hr(),
            menuItem("User Manual", tabName = "user_manual", icon = icon("book")),
            hr(),
            h2("System Administration", style = "vertical-align: middle; margin-left: 40px; font-size: 14px; color: #17d0ff; font-weight: bold; "),
            hr(),
            menuItem("Settings",
              tabName = "settings", icon = icon("cogs"),
              conditionalPanel(
                condition = "output.is_admin",
                menuSubItem("Source Setting", tabName = "source_setting", icon = icon("sliders-h")),
                menuSubItem("Fetcher Setting", tabName = "fetcher_setting", icon = icon("download")),
              )
            ),
            menuItem("Data Management",
              icon = icon("database"),
              conditionalPanel(
                condition = "output.is_admin",
                menuSubItem("Data Cleaner", tabName = "dash_explore_clean", icon = icon("broom")), # ,
                # menuSubItem("Explore & Clean", tabName = "explore_clean", icon = icon("broom"))
              )
            ),
#            hr(),
            menuItem("Admin Panel",
              tabName = "admin_panel", icon = icon("users"),
              conditionalPanel(
                condition = "output.is_admin",
                menuSubItem("User Management", tabName = "user_management"),
                menuSubItem("Role Management", tabName = "role_management"),
                menuSubItem("Database Management", tabName = "db_management"),
              )
            )
#            hr(),
#            menuItem("User Manual", tabName = "user_manual", icon = icon("book"))
          ),
          hr(),
          tags$html(
            useShinyjs(), # Activate shinyjs functionality
            class = "pdropdown user-menu",
            uiOutput("user_profile")
          )
        ),
        dashboardBody(
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
            # Add modern fonts
            tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap", rel = "stylesheet"),
            tags$style(HTML("
        body {
          font-family: 'Roboto', sans-serif;
        }
      "))
          ),
          useShinyjs(),
          class = "dashboard-body",
          tags$head(tags$style(HTML(css))),
          tags$script(HTML("
  $(document).ready(function() {
    // Add scroll behavior to header
    $(window).scroll(function() {
      if ($(this).scrollTop() > 20) {
        $('.main-header').addClass('scrolled');
      } else {
        $('.main-header').removeClass('scrolled');
      }
    });

    // Enhanced hover effects
    $('.main-header .dropdown a').hover(
      function() {
        $(this).css('transform', 'translateY(-2px)');
        $(this).find('i').css({
          'transform': 'scale(1.1)',
          'color': '#b5e71c'
        });
      },
      function() {
        $(this).css('transform', 'translateY(0)');
        $(this).find('i').css({
          'transform': 'scale(1)',
          'color': 'inherit'
        });
      }
    );

    // Pulse animation for active items
    setInterval(function() {
      $('.navbar-nav > li.active > a').css('animation', 'pulse 2s infinite');
    }, 3000);

    // Smooth dropdown animations
    $('.dropdown').on('show.bs.dropdown', function() {
      $(this).find('.dropdown-menu').first().stop(true, true).slideDown(200);
    });

    $('.dropdown').on('hide.bs.dropdown', function() {
      $(this).find('.dropdown-menu').first().stop(true, true).slideUp(200);
    });

    // Mobile menu toggle enhancement
    $('.sidebar-toggle').click(function() {
      $(this).toggleClass('active');
      $('.main-header').toggleClass('menu-open');

        $('.user-info').click(function() {
          var $menu = $(this).siblings('.pdropdown-menu');
          // Close any open dropdowns and toggle the current one
          if ($menu.is(':visible')) {
            $menu.slideUp(200);
          } else {
            // Close all dropdowns first
            $('.pdropdown-menu').slideUp(200);
            $menu.stop(true, true).slideDown(200);
          }
        });

        // Prevent dropdown closing when clicking inside the menu
        $('.pdropdown-menu').click(function(e) {
          e.stopPropagation();

    });
  });
")),
          tabItems(


            # user admin panel tabs
            # user admin panel tabs
            tabItem(
              tabName = "user_management",
              fluidRow(
                box(
                  title = "User Management",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("add_user", "Add User", class = "btn-success"),
                  DTOutput("user_table"),
                  actionButton("edit_user", "Edit User", class = "btn-primary"),
                  actionButton("delete_user", "Delete User", class = "btn-danger")
                )
              )
            ),
            tabItem(
              tabName = "role_management",
              fluidRow(
                box(
                  title = "Role Management",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                      h4("Permissions Management"),
                      textInput("new_permission", "New Permission Name"),
                      actionButton("add_permission", "Add Permission", class = "btn-success"),
                      br(), br(),
                      DTOutput("permissions_list")
                    ),
                    column(4,
                      h4("Role Management"),
                      textInput("new_role_name", "New Role Name"),
                      actionButton("add_role", "Create Role", class = "btn-primary"),
                      br(), br(),
                      selectInput("role_select", "Select Role", choices = NULL),
                      actionButton("delete_role", "Delete Role", class = "btn-danger")
                    ),
                    column(4,
                      h4("Role Permissions"),
                      uiOutput("permissions_ui"),
                      actionButton("save_permissions", "Save Permissions", class = "btn-warning")
                    )
                  )
                )
              )
            ),


            # Database Management tab

            tabItem(
              tabName = "db_management",
              fluidRow(
                box(
                  title = "Database Administration",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  conditionalPanel(
                    condition = "output.is_admin",
                    dba_module_ui("dba_module")
                  ),
                  conditionalPanel(
                    condition = "!output.is_admin",
                    h3("Access Denied"),
                    p("You don't have permission to access this section")
                  )
                )
              )
            ),

            # other tab items


            tabItem(
              tabName = "source_setting",
              fluidRow(
                box(
                  title = "Source Setting",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  textInput("setting", "Setting", value = "Ethiopia"),
                  textInput("source", "Source", value = "DHIS_2"),
                  textInput("iso3", "ISO3", value = "ETH"),
                  conditionalPanel(
                    # condition = "output.role == 'mikeintosh'",
                    condition = "output.is_admin",
                    actionButton("save_source_settings", "Save Source Settings", class = "btn-primary")
                  ),
                  actionButton("load_source_settings", "Load Source Settings", class = "btn-info")
                )
              )
            ),
            tabItem(
              tabName = "fetcher_setting",
              fluidRow(
                box(
                  title = "Fetcher Setting",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  conditionalPanel(
                    # condition = "output.role == 'mikeintosh'",
                    condition = "output.is_admin",
                    actionButton("save_settings", "Save Settings", class = "btn-primary"),
                    textInput("base_url", "DHIS2 Base URL", value = Sys.getenv("DHIS2_BASE_URL")),
                    textInput("username", "Username", value = Sys.getenv("DHIS2_USERNAME")),
                    passwordInput("password", "Password", value = Sys.getenv("DHIS2_PASSWORD"))
                  ),
                  actionButton("load_settings", "Load Settings", class = "btn-info"),
                  actionButton("fetch_data", "Fetch Data", class = "btn-success"),
                  downloadButton("export_data", "Export to Excel", class = "btn-warning"),
                  downloadButton("export_parquet", "Export to Parquet", class = "btn-danger"),
                  actionButton("help", "Help", class = "btn-info"),
                  hr(),
                  checkboxInput("select_all_indicators", "Select All Indicators", value = FALSE),
                  selectizeInput("indicators", "Select Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                  textInput("indicator_abbr", "Indicator Abbreviations (comma-separated)", value = ""),
                  selectizeInput("favorable_indicators", "Favorable Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                  hr(),
                  conditionalPanel(
                    condition = "input.custom_indicator_scales == true",
                    hr(),
                    h4("Ordered Dimension Settings"),
                    selectizeInput("ordered_indicators", "Select Indicators for Ordered Dimensions",
                      choices = NULL, multiple = TRUE
                    ),
                    uiOutput("ordered_dimensions_ui"),
                    actionButton("save_ordered_settings", "Save Ordered Settings", class = "btn-info")
                  ),
                  hr(),
                  checkboxInput("custom_indicator_scales", "Set Custom Indicator Scales", value = TRUE),
                  conditionalPanel(
                    condition = "input.custom_indicator_scales == true",
                    selectizeInput("custom_indicators", "Select Indicators for Custom Scales", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                    uiOutput("custom_scales_ui")
                  ),
                  hr(),
                  checkboxInput("select_all_org_units", "Select All Organisation Units", value = FALSE),
                  selectizeInput(
                    "org_units", 
                    "Select Organisation Units", 
                    choices = NULL, 
                    multiple = TRUE, 
                    options = list(
                      placeholder = 'Select organisation units',
                      maxOptions = 1000,
                      plugins = list('remove_button')
                    )
                  ),
                  hr(),
                  checkboxInput("select_all_zones", "Select All Zones", value = FALSE),
                  selectizeInput(
                    "zones", 
                    "Select Zones", 
                    choices = NULL, 
                    multiple = TRUE, 
                    options = list(
                      placeholder = 'Select zones',
                      maxOptions = 1000,
                      plugins = list('remove_button')
                    )
                    # Remove server = TRUE from here
                  ),
                  hr(),
                  checkboxInput("select_all_woredas", "Select All Woredas", value = FALSE),
                  selectizeInput(
                    "woredas", 
                    "Select Woredas", 
                    choices = NULL, 
                    multiple = TRUE, 
                    options = list(
                      placeholder = 'Select woredas',
                      maxOptions = 1000,
                      plugins = list('remove_button')
                    )
                    # Remove server = TRUE from here
                  ),
                  hr(),
                  # In the fetcher_setting tab box, after Woredas section
                  hr(),
                  checkboxInput("select_all_facilities", "Select All Facility Types", value = FALSE),
                  selectizeInput("facility_types", "Select Facility Types",
                    choices = list(
                      "Clinics" = "kwcNbI9fPdB",
                      "Health Centers" = "j8SCxUTyzfm",
                      "Health Post" = "FW4oru60vgc",
                      "Hospitals" = "nVEDFMfnStv"
                    ),
                    multiple = TRUE,
                    options = list(maxOptions = 1000)
                  ),
                  hr(),
                  checkboxInput("select_all_settlements", "Select All Settlement Types", value = FALSE),
                  selectizeInput("settlement_types", "Select Settlement Types",
                    choices = list(
                      "Urban Settlement" = "nKT0uoFbxdf",
                      "Pastoral Settlement" = "ZktuKijP5jN",
                      "Agrarian Settlement" = "V9sleOboZJ1"
                    ),
                    multiple = TRUE,
                    options = list(maxOptions = 1000)
                  ),
                  hr(),
                  textInput("periods", "Periods (comma-separated)", value = ""),
                  hr(),
                  hr()
                )
              )
            ),
            tabItem(
              tabName = "data_preview",
              fluidRow(
                # actionButton("load_settings", "Load Settings", class = "btn-info"),
                # actionButton("fetch_data", "Fetch Data", class = "btn-success"),
                downloadButton("export_data", "Export All To Excel", class = "btn-warning"),
                # downloadButton("export_parquet", "Export to Parquet", class = "btn-danger"),
                actionButton("help", "Help", class = "btn-info"),
                hr(),
                box(
                  title = tagList(
                    "Filters",
                    p("To get started, select indicators and dimensions, then apply filters",
                      style = "font-size: 14px; color: #b7b7b7;"
                    )
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(3, selectizeInput("filter_indicators", "Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
                    column(3, selectizeInput("filter_dimensions", "Dimensions",
                      choices = c("Region", "Zone", "Woreda", "Facility Type", "Settlement"),
                      multiple = TRUE, options = list(maxOptions = 1000)
                    )),
                    column(
                      3,
                      conditionalPanel(
                        condition = "input.filter_dimensions.includes('Zone') || input.filter_dimensions.includes('Woreda')",
                        selectizeInput("filter_regions", "Regions", choices = NULL, multiple = TRUE)
                      ),
                      conditionalPanel(
                        condition = "input.filter_dimensions.includes('Woreda')",
                        selectizeInput("filter_zones_woreda", "Zones", choices = NULL, multiple = TRUE)
                      )
                    ),
                    column(3, selectizeInput("filter_subgroups", "Subgroups", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
                    column(3, selectizeInput("filter_dates", "Dates", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),

                    #    column(12, actionButton("apply_filters", "Apply Filters", class = "btn-primary", style = "float: right;")),
                    column(12, actionButton("apply_filters", "Apply Filters", class = "btn-primary", style = "float: right;"))
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Histogram Plot",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotlyOutput("distPlot", width = "100%", height = "380px")
                  ),
                  box(
                    title = "Geographical Heatmap",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = 900,
                    p("Regional map visualization based on current filters",
                      style = "font-size: 12px; color: #666; margin-bottom: 10px;"
                    ),
                    ethgeoUI("ethgeo_module"),
                    hr()
                  )
                ),
                column(
                  width = 6,
                  box(
                    title = "Dynamic Plot Settings",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    fluidRow(
                      column(4, colourInput("plot_color", "Marker Color", value = "#75FF09")),
                      column(4, numericInput("marker_size", "Marker Size", value = 7, min = 1, max = 10)),
                      column(4, conditionalPanel(
                        condition = "input.chart_type == 'Scatter'",
                        selectInput("plot_mode", "Plot Mode",
                          choices = c(
                            "Markers" = "markers",
                            "Lines+Markers" = "lines+markers"
                            #"Lines" = "lines"
                          )
                        ),
                        colourInput("line_color", "Line Color", value = "#075E57")
                      ))
                    )
                  ),
                  box(
                    title = "Dynamic Plot & Visualizations",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = "984px",
                    selectInput("view_by", "Dynamic Plot:", choices = c("Subgroup", "Dimension", "Date")),
                    # selectInput("view_by", "Dynamic Plot:", choices = c("Subgroup", "Dimension")),
                    selectInput("chart_type", "Chart Type:",
                      # choices = c("Scatter", "Bar", "Geo Heatmap", "Pie", "Heatmap"), selected = "Scatter"),
                      # choices = c("Scatter", "Bar"), selected = "Scatter"),
                      choices = c("Scatter", "Bar", "Geo Heatmap", "Heatmap", "Pie"), selected = "Scatter"
                    ),
                    uiOutput("dynamicPlotUI"), # Dynamic UI for plots
                    plotlyOutput("dynamicPlotOutput", width = "100%", height = "700px")
                  )
                ),

                # Move Data Preview to the bottom with full width
                box(
                  title = "Data Preview",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12, # Full width
                  withSpinner(DTOutput("data_preview"))
                )
              )
            ),
            tabItem(
              tabName = "benchmarking",
              tags$head(
                tags$style(HTML("
      .map-container {
        height: 800px;
        width: 100%;
      }
    "))
              ),
              fluidRow(
                box(
                  title = "Benchmark Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(
                      3,
                      selectInput("benchmark_setting", "Setting Level",
                        choices = c("Country", "Region", "Zone", "Woreda"),
                        selected = "Region"
                      )
                    ),
                  ),
                  conditionalPanel(
                    condition = "input.benchmark_setting == 'Country'",
                    fluidRow(
                      column(
                        3,
                        selectizeInput("income_filter", "Country Income Group",
                          choices = NULL,
                          multiple = TRUE
                        )
                      ),
                      column(
                        3,
                        selectizeInput("who_region_filter", "WHO Region",
                          choices = NULL,
                          multiple = TRUE
                        )
                      ),
                      column(
                        3,
                        selectizeInput("country_select", "Select Countries",
                          choices = NULL,
                          multiple = TRUE
                        )
                      ),
                      column(
                        3,
                        selectizeInput("benchmark_indicator_country", "Indicator",
                          choices = NULL, multiple = TRUE
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        4,
                        selectizeInput("benchmark_dimension_country", "Dimension",
                          choices = NULL, multiple = TRUE
                        )
                      ),
                      column(
                        4,
                        selectizeInput("benchmark_subgroup_country", "Reference Subgroup",
                          choices = NULL, multiple = FALSE
                        )
                      ),
                      column(
                        4,
                        conditionalPanel(
                          condition = "input.benchmark_date_type == 'Date Range'",
                          numericInput("start_year", "Start Year",
                            value = 2014, min = 2000, max = 2100
                          ),
                          numericInput("end_year", "End Year",
                            value = as.integer(format(Sys.Date(), "%Y")),
                            min = 2000, max = 2100
                          )
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.benchmark_setting != 'Country'",
                    fluidRow(
                      column(
                        4,
                        selectizeInput("benchmark_indicator", "Indicator",
                          choices = NULL, multiple = FALSE
                        )
                      ),
                      column(
                        4,
                        selectizeInput("benchmark_dimension", "Inequality Dimension",
                          choices = NULL,
                          selected = "Region"
                        )
                      ),
                      column(
                        4,
                        selectizeInput("benchmark_subgroup", "Benchmark Subgroup",
                          choices = NULL, multiple = TRUE
                        )
                      )
                    )
                  ),
                  fluidRow(
                    # column(
                    #   3,
                    #   selectInput("benchmark_date_type", "Date Selection",
                    #     choices = c("Single Date", "Date Range"),
                    #     selected = "Single Date"
                    #   )
                    # ),
                    # column(
                    #   3,
                    #   conditionalPanel(
                    #     condition = "input.benchmark_date_type == 'Date Range'",
                    #     numericInput("start_year", "Start Year",
                    #       value = 2014, min = 2000, max = 2100
                    #     ),
                    #     numericInput("end_year", "End Year",
                    #       value = as.integer(format(Sys.Date(), "%Y")),
                    #       min = 2000, max = 2100
                    #     )
                    #   )
                    # ),
                    # column(
                    #   3,
                    #   conditionalPanel(
                    #     condition = "input.benchmark_date_type == 'Single Date'",
                    #     selectizeInput("benchmark_specific_date", "Select Date",
                    #     choices = NULL, multiple = TRUE)
                    #   )
                    # ),
                    column(
                      4,
                      selectInput("benchmark_date_type", "Date Selection",
                        choices = c("Multiple Dates", "Date Range"),
                        selected = "Multiple Dates"
                      ),
                      conditionalPanel(
                        condition = "input.benchmark_date_type == 'Multiple Dates'",
                        selectizeInput("benchmark_specific_date", "Select Date(s)",
                          choices = NULL,
                          multiple = TRUE
                        )
                      ),
                      conditionalPanel(
                        condition = "input.benchmark_date_type == 'Date Range'",
                        numericInput("start_year", "Start Year",
                          value = 2014, min = 1990, max = 2100
                        ),
                        numericInput("end_year", "End Year",
                          value = as.integer(format(Sys.Date(), "%Y")),
                          min = 1990, max = 2100
                        )
                      )
                    ),
                    #                          conditionalPanel(
                    #                            condition = "input.benchmark_date_type == 'Multiple Dates'",
                    #                          selectizeInput("benchmark_specific_date", "Select Dates",
                    #                            choices = NULL, multiple = TRUE
                    #                          ))
                    #                        ),
                    # column(
                    #   3,
                    #   conditionalPanel(
                    #     condition = "input.benchmark_chart_type == 'Bar' || input.benchmark_chart_type == 'Horizontal Bar'",
                    #     selectInput("benchmark_bar_mode", "Bar Mode",
                    #       choices = c("Grouped" = "group", "Stacked" = "stack"),
                    #       selected = "group"
                    #     )
                    #   )
                    # ),
                    # column(
                    #   3,
                    #   numericInput("plot_height", "Plot Height (px)", value = 600, min = 300, max = 1200)
                    # ),
                    # column(
                    #   3,
                    #   numericInput("plot_width", "Plot Width (px)", value = 900, min = 300, max = 1200)
                    # ),
                    column(
                      4,
                      actionButton("apply_benchmark", "Apply Benchmark", class = "btn-primary")
                    )
                  )
                ),
                box(
                  title = "Benchmark Comparison",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  tabsetPanel(
                    tabPanel(
                      "Visual Comparison",
                      plotlyOutput("benchmark_plot",
                        height = "600px",
                        width = "100%"
                      )
                    ),
                    tabPanel(
                      "Dynamic Pivot Table",
                      rpivotTableOutput("pivot_table",
                        height = "900px",
                        width = "100%"
                      )
                    ),
                    tabPanel(
                      "Data Table",
                      DTOutput("benchmark_table")
                    ),
                    tabPanel(
                      "Summary Statistics",
                      verbatimTextOutput("benchmark_summary")
                    ),
                    tabPanel(
                      "Map Visualization",
                      div(
                        class = "map-container",
                        plotlyOutput("benchmark_map", height = "100%")
                      ),
                      checkboxInput("show_average", "Show WHO Region Average", FALSE),
                      checkboxInput("show_income", "Show Income Group Average", FALSE)
                    ),
                    tabPanel(
                      "Comparison Table",
                      downloadButton("download_benchmark_data", "Download Data"),
                      DT::dataTableOutput("benchmark_comparison_table")
                    )
                  ),
                  downloadButton("download_benchmark", "Download Benchmark Data",
                    class = "btn-info"
                  )
                )
              )
            ),
            tabItem(
              tabName = "summary_measures",
              fluidRow(
                box(
                  title = "Summary Measures Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4, selectInput("sm_indicator", "Select Indicator", choices = NULL)),
                    column(4, selectInput("sm_dimension", "Select Dimension", choices = NULL)),
                    column(4, selectInput("sm_date", "Select Date", choices = NULL))
                  ),
                  fluidRow(
                    column(4, checkboxInput("sm_weighted", "Weighted Measures", value = TRUE)),
                    column(4, selectInput("sm_reference", "Reference Subgroup", choices = NULL)),
                    column(4, actionButton("sm_calculate", "Calculate Measures", class = "btn-primary"))
                  )
                ),
                box(
                  title = "Summary Measures Results",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  tabsetPanel(
                    tabPanel(
                      "Simple Measures",
                      fluidRow(
                        column(
                          6,
                          h4("Difference (D)"),
                          verbatimTextOutput("sm_difference"),
                          highchartOutput("sm_difference_plot", height = "300px") %>%
                            withSpinner(type = 6, color = "#3c8dbc")
                        ),
                        column(
                          6,
                          h4("Ratio (R)"),
                          verbatimTextOutput("sm_ratio"),
                          highchartOutput("sm_ratio_plot", height = "300px")
                        )
                      )
                    ),
                    tabPanel(
                      "Ordered Disproportionality",
                      fluidRow(
                        column(
                          6,
                          h4("Absolute Concentration Index (ACI)"),
                          verbatimTextOutput("sm_aci"),
                          highchartOutput("sm_aci_plot", height = "300px")
                        ),
                        column(
                          6,
                          h4("Relative Concentration Index (RCI)"),
                          verbatimTextOutput("sm_rci"),
                          highchartOutput("sm_rci_plot", height = "300px")
                        )
                      )
                    ),
                    tabPanel(
                      "Regression-Based",
                      fluidRow(
                        column(
                          6,
                          h4("Slope Index of Inequality (SII)"),
                          verbatimTextOutput("sm_sii"),
                          highchartOutput("sm_sii_plot", height = "300px")
                        ),
                        column(
                          6,
                          h4("Relative Index of Inequality (RII)"),
                          verbatimTextOutput("sm_rii"),
                          highchartOutput("sm_rii_plot", height = "300px")
                        )
                      )
                    ),
                    tabPanel(
                      "Variance Measures",
                      fluidRow(
                        column(
                          4,
                          h4("Between-Group Variance (BGV)"),
                          verbatimTextOutput("sm_bgv")
                        ),
                        column(
                          4,
                          h4("Between-Group SD (BGSD)"),
                          verbatimTextOutput("sm_bgsd")
                        ),
                        column(
                          4,
                          h4("Coefficient of Variation (COV)"),
                          verbatimTextOutput("sm_cov")
                        )
                      ),
                      highchartOutput("sm_variance_plot", height = "300px")
                    ),
                    tabPanel(
                      "Mean Difference",
                      fluidRow(
                        column(
                          4,
                          h4("From Mean (Unweighted)"),
                          verbatimTextOutput("sm_mdmu")
                        ),
                        column(
                          4,
                          h4("From Mean (Weighted)"),
                          verbatimTextOutput("sm_mdmw")
                        ),
                        column(
                          4,
                          h4("From Best (Unweighted)"),
                          verbatimTextOutput("sm_mdbu")
                        )
                      ),
                      fluidRow(
                        column(
                          4,
                          h4("From Best (Weighted)"),
                          verbatimTextOutput("sm_mdbw")
                        ),
                        column(
                          4,
                          h4("From Reference (Unweighted)"),
                          verbatimTextOutput("sm_mdru")
                        ),
                        column(
                          4,
                          h4("From Reference (Weighted)"),
                          verbatimTextOutput("sm_mdrw")
                        )
                      ),
                      highchartOutput("sm_meandiff_plot", height = "300px")
                    ),
                    tabPanel(
                      "Disproportionality",
                      fluidRow(
                        column(
                          6,
                          h4("Theil Index (TI)"),
                          verbatimTextOutput("sm_ti"),
                          highchartOutput("sm_ti_plot", height = "300px")
                        ),
                        column(
                          6,
                          h4("Mean Log Deviation (MLD)"),
                          verbatimTextOutput("sm_mld"),
                          highchartOutput("sm_mld_plot", height = "300px")
                        )
                      )
                    ),
                    tabPanel(
                      "Impact Measures",
                      fluidRow(
                        column(
                          6,
                          h4("Population Attributable Fraction (PAF)"),
                          verbatimTextOutput("sm_paf"),
                          highchartOutput("sm_paf_plot", height = "300px")
                        ),
                        column(
                          6,
                          h4("Population Attributable Risk (PAR)"),
                          verbatimTextOutput("sm_par"),
                          highchartOutput("sm_par_plot", height = "300px")
                        )
                      )
                    ),
                    tabPanel(
                      "Data Table",
                      DTOutput("sm_data_table")
                    )
                  )
                )
              )
            ),
            tabItem(
              tabName = "geographical",
              spatialUI("spatial_module")
            ),
            tabItem(
              tabName = "dash_explore_clean",
              fluidPage(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css") # Link to external CSS
                ),
                titlePanel(div(class = "title-panel", "Data Cleaning Dashboard")),
                fluidRow(
                  column(
                    width = 12,
                    div(
                      class = "horizontal-sidebar",
                      selectInput("na_action", "Handle Missing Values:",
                        choices = c("None", "Remove Rows", "Replace with Mean", "Replace with Median", "Replace with Mode")
                      ),
                      actionButton("apply_na", "Apply", class = "btn"),
                      checkboxInput("remove_dupes", "Remove Duplicates", FALSE),
                      actionButton("apply_dupes", "Apply", class = "btn"),
                      uiOutput("col_select"),
                      selectInput("convert_type", "Convert Data Type:",
                        choices = c("None", "Numeric", "Character", "Factor", "Date")
                      ),
                      actionButton("apply_convert", "Apply", class = "btn"),
                      uiOutput("rename_ui"), # Dynamically generated rename UI
                      actionButton("apply_rename", "Apply Rename", class = "btn"),
                      numericInput("zscore_threshold", "Z-score Outlier Threshold", value = 3, min = 1, max = 10, step = 0.1),
                      actionButton("detect_outliers", "Detect Outliers", class = "outbtn"),
                      actionButton("save_clean", "Save Cleaned Data", class = "savebtn"),
                      downloadButton("download_clean", "Download Cleaned Data", class = "downbtn")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    tabsetPanel(
                      tabPanel("Data Preview", DTOutput("clean_table")),
                      tabPanel("Outlier Detection", DTOutput("outliers"))
                    )
                  )
                )
              )
            ),
            tabItem(
              tabName = "explore_clean",
              # h2("Data Management"),
              hr(),
              uiOutput("exclean_ui"),
              hr(),
              fluidRow(
                # uiOutput("exclean_ui"),
                box(
                  title = "Data Management",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Data Cleansing or Data Wrangling is an important early step in the data analytics process."),
                  # Dynamically render the exclean UI here
                  # uiOutput("exclean_ui")
                  # uiOutput("mDataExplorationUI")
                )
              )
            ),
            tabItem(
              tabName = "user_manual",
              manualUI("manual_module")
            )
          ),
          hr(),
          hr(),
          div(
            class = "footer",
            HTML("&copy; 2025 Designed & Developed by: <a href='https://merqconsultancy.org'><b>MERQ Consultancy</b>.</a>")
          )
        ),
        tags$head(
          tags$title("DHIS2 Data Fetcher for HEAT Plus(+) - Dashboard")
        )
      )
    )
  )
)



# Run the application
# shinyApp(ui = ui, server = server)
# shinyApp(ui = ui, server = server)
