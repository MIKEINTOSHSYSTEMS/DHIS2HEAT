library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinyjs)
library(shinyBS)
library(shinyalert)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(colorspace)
library(pastecs)
library(shinycssloaders)
library(gridlayout)
library(bslib)
library(colourpicker)
library(httr)
library(openxlsx)
library(tidyr)
library(arrow)
library(rjson)
library(jsonlite)
library(slickR)
library(rpivotTable)
library(purrr)
library(viridisLite)
library(sf)
library(highcharter)
library(DT)

# Source all modules
source("load_countries.R", local = TRUE)
source("manual.R", local = TRUE)
source("spatial_module.R", local = TRUE)
source("exclean.R", local = TRUE)
source("ethgeo.R", local = TRUE)
source("dba.R", local = TRUE)

# Load UI and server
source("ui.R", local = TRUE)
source("server.R", local = TRUE)


# setwd("/srv/shiny-server/UI")

# Source the UI and server components
# source("system.R") # Source the standalone database administration module
#source("spatial.R")
#source("./module/spatial_module.R")
# source("clean.R")
# source("auth.R")

# Run the application
shinyApp(ui = ui, server = server)

# Run the application with configurable port

# shiny::runApp("app.R", port = 3838, host = "0.0.0.0")

# shiny::runApp("./app/app.R", port = 3838, host = "0.0.0.0")

# shiny::runApp("./app/manual.R", port = 3838, host = "0.0.0.0")

# shiny::runApp("./app/converter.R", port = 3838, host = "0.0.0.0")

# shiny::runApp("./app/spatial.R", port = 3838, host = "0.0.0.0")
# shiny::runApp("./app/spatials.R", port = 3838, host = "0.0.0.0")

# shinyApp(ui = ui, server = server, options = list(port = as.numeric(Sys.getenv("SHINY_PORT", 3838))))

# shiny::runApp("./UI/app.R", port = 3838, host = "0.0.0.0")
# launch_editor(app_loc = "./UI/app.R")
# shinyuieditor::launch_editor(app_loc = "./UI/app.R")
# source("UI/auth.R")
# PRODUCTION
# shiny::runApp("./UI/app.R", port = 3939, host = "0.0.0.0")

# jsonlite Package issues 
# packageVersion("jsonlite")
# detach("package:jsonlite", unload = TRUE)
# install.packages("jsonlite")

# In RStudio:
#.rs.restartR() # Keyboard shortcut: Ctrl+Shift+F10

# In plain R:
#quit("no") # Then restart R manually
# 

