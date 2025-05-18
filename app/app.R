library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(gridlayout)
library(bslib)
library(plyr) # empty() function is from this package
library(dplyr) # select functions are covered in the require
library(DT) # for using %>% which works as a pipe in R code
library(rjson)
library(jsonlite)

# setwd("/srv/shiny-server/UI")

# Source the UI and server components
# source("system.R") # Source the standalone database administration module
source("load_countries.R")
source("spatial_module.R")
source("manual.R")
#source("spatial.R")
#source("./module/spatial_module.R")
source("dba.R") # Source the database administration module
source("exclean.R")
# source("clean.R")
source("ethgeo.R")
source("ui.R")
source("server.R")
# source("auth.R")



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

