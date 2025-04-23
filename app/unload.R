# unload.R — Unloads all packages listed in install.R if loaded

# List of packages to unload (match install.R)
packages_to_unload <- c(
    "shiny",
    "shinydashboard",
    "shinyWidgets",
    "shinyjs",
    "shinyBS",
    "shinyalert",
    "shinycssloaders",
    "dplyr",
    "DT",
    "plotly",
    "scales",
    "pastecs",
    "car",
    "ggfortify",
    "arrow",
    "psych",
    "sqldf",
    "lubridate",
    "kableExtra",
    "gridExtra",
    "reshape2",
    "fastDummies",
    "DataExplorer",
    "tidyverse",
    "colorspace",
    "here",
    "rjson",
    "purrr",
    "RSQLite",
    "sodium",
    "DBI",
    "openxlsx",
    "slickR",
    "remotes",
    "httr",
    "leaflet",
    "sf",
    "rnaturalearth",
    "rpivotTable",
    "RPostgreSQL"
)

# Function to safely detach packages
safe_detach <- function(pkg) {
    pkg_name <- paste0("package:", pkg)
    if (pkg_name %in% search()) {
        tryCatch(
            {
                detach(pkg_name, unload = TRUE, character.only = TRUE)
                message(sprintf("Detached %s", pkg))
            },
            error = function(e) {
                message(sprintf("Could not detach %s: %s", pkg, e$message))
            }
        )
    }
}

# Detach each package if loaded
invisible(lapply(packages_to_unload, safe_detach))

# Optionally clear other loaded namespaces
cat("\nAttempting to unload namespaces (if not attached by others)...\n")
for (pkg in packages_to_unload) {
    if (pkg %in% loadedNamespaces()) {
        tryCatch(
            {
                unloadNamespace(pkg)
                message(sprintf("Unloaded namespace: %s", pkg))
            },
            error = function(e) {
                message(sprintf("Namespace '%s' could not be unloaded: %s", pkg, e$message))
            }
        )
    }
}

# Optional: clear environment and memory
cat("\nClearing global environment and memory...\n")
rm(list = ls())
gc()

cat("\nUnload script completed.\n")

# Final message for VSCode users
cat("To fully clear session, please restart your R process in VSCode (kill R terminal and start again).\n")

# Optional prompt to quit R session
if (interactive()) {
    answer <- readline("\nWould you like to quit R now? [y/n]: ")
    if (tolower(answer) == "y") quit(save = "no")
}
