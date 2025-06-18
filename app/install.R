# install.R script to install required packages

# List of required packages
required_packages <- c(
    "shiny",
    "shinydashboard",
    "shinyWidgets", # This can be kept alongside shiny
    "shinyjs", # This can be kept
    "shinyBS",
    "shinyalert", # These are all for enhancing Shiny functionality
    "shinycssloaders",
    "dplyr", # Included in tidyverse
    "DT", # For data tables
    "plotly", # Interactive plots
    "scales", # For scaling functionality in plots
    "pastecs", # Descriptive statistics
    "car", # Regression diagnostics
    "ggfortify", # For visualizing model results
    "arrow", # Arrow data format support
    "psych", # Psychological and psychometric functions
    "sqldf", # You might not need this if using DBI + RSQLite
    "lubridate", # Date-time manipulation
    "kableExtra", # For nice table formatting
    "gridExtra", # For combining grid-based graphics
    "reshape2", # Deprecated in favor of tidyr, so consider removing
    "fastDummies", # For dummy variable creation
    "DataExplorer", # For data exploration
    "tidyverse", # Includes dplyr, ggplot2, tidyr, etc.
    "colorspace", # Color utilities
    "here", # File path management
    "rjson", # JSON parsing
    "purrr", # For functional programming
    "RSQLite", # If using SQLite
    "sodium", # Cryptography and hashing
    "DBI", # General database interface
    "openxlsx",
    #"slickR", # For slick slider UI elements in Shiny | Currently removed from cran
    "remotes", # Add remotes if needed for installing from remote repositories
    "httr", # Add httr if needed for HTTP requests
    "leaflet",
    "highcharter", # For highcharts integration
    "viridisLite", # Color palettes
    "sf",
    "rnaturalearth",
    "rpivotTable",
    "RPostgreSQL", # For PostgreSQL database connection
    "DBI", # General database interface
    "htmltools",
    "pagedown",
    "readxl",
    "writexl",
    "markdown",
    "rmarkdown",
    "stringr",
    "tinytex"


)

# Remove dplyr and ggplot2 since they're part of tidyverse
# Remove plyr since dplyr covers similar functionality
# Remove reshape2 (deprecated)

# Install packages if they are not already installed
install_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
    }
}

# Loop through each package and install if missing
lapply(required_packages, install_missing)

# Install GitHub packages
cat("Installing GitHub packages...\n")
remotes::install_github(c("rstudio/gridlayout", "tidyverse/ggplot2", "thomasp85/patchwork", "LukasK13/SummeRnote", "yonicd/slickR"))
#remotes::install_github(c("rstudio/gridlayout", "tidyverse/ggplot2", "thomasp85/patchwork"))

# Start installation of TinyTeX
# Install TinyTeX if not already installed
if (!tinytex::is_tinytex()) {
  cat("Installing TinyTeX...\n")
  tinytex::install_tinytex()
  if (tinytex::is_tinytex()) {
    cat("TinyTeX installed successfully.\n")
    # Install LaTeX packages using tinytex
    tinytex::tlmgr_install("collection-latexextra") # Or specific packages like "longtable"
  } else {
    cat("TinyTeX installation failed. Please check for errors.\n")
  }
} else {
  cat("TinyTeX is already installed.\n")
}
# End installation of TinyTeX

# Print a message when installation is complete
cat("All required packages are installed.\n")


# to install
# source("install.R")
# source("UI/install.R")

#install.packages("package_name")
# install.packages("DBI")
# install.packages("RPostgreSQL")
# devtools::install_github("LukasK13/SummeRnote")
# Additional ToDos is create NAMESPACE using roxygen2 
#for auto loading of packages and also renv.lock

# Install TinyTeX for LaTeX support in R Markdown and other documents
# This is necessary for rendering PDF documents and reports

# Install TinyTeX if not already installed
#tinytex::install_tinytex()
# Check if TinyTeX is installed
#tinytex::is_tinytex()
# Should return TRUE otherwise install TinyTeX

# Install LaTeX packages using tinytex
#tinytex::tlmgr_install("collection-latexextra")

# Has a count of 2024 file attribs that Will installed
