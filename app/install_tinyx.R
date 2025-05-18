# install_tinytex.R - Complete TinyTeX installation script

# Function to check if a command exists
command_exists <- function(cmd) {
    Sys.which(cmd) != ""
}

# Install TinyTeX with robust error handling
install_tinytex_complete <- function() {
    # Check if TinyTeX package is installed
    if (!requireNamespace("tinytex", quietly = TRUE)) {
        message("Installing tinytex R package...")
        install.packages("tinytex", repos = "https://cloud.r-project.org/")
    }

    # Check if TinyTeX is already installed
    if (tinytex::is_tinytex()) {
        message("TinyTeX is already installed.")
        return(TRUE)
    }

    message("Installing TinyTeX distribution...")

    # Try different installation methods
    tryCatch(
        {
            # Method 1: Standard installation
            tinytex::install_tinytex()

            # Verify installation
            if (!tinytex::is_tinytex()) stop("Installation verification failed")

            message("TinyTeX installed successfully.")
            return(TRUE)
        },
        error = function(e) {
            message("Standard installation failed: ", e$message)

            # Method 2: Alternative installation
            tryCatch(
                {
                    message("Trying alternative installation method...")

                    # For Windows
                    if (.Platform$OS.type == "windows") {
                        utils::download.file(
                            "https://yihui.org/tinytex/TinyTeX-1.zip",
                            destfile = "TinyTeX-1.zip"
                        )
                        utils::unzip("TinyTeX-1.zip")
                        tinytex::use_tinytex()
                        # tinytex::tinytex_root()
                    }
                    # For Unix-like systems
                    else {
                        system("wget -qO- https://yihui.org/tinytex/install-bin-unix.sh | sh")
                    }

                    # Verify installation
                    if (!tinytex::is_tinytex()) stop("Alternative installation failed")

                    message("TinyTeX installed successfully via alternative method.")
                    return(TRUE)
                },
                error = function(e2) {
                    message("Alternative installation failed: ", e2$message)
                    return(FALSE)
                }
            )
        }
    )
}

# Install required packages with retries
install_required_packages <- function() {
    required_pkgs <- c(
        "geometry", "hyperref", "ulem", "amsmath", "lineno", "fancyhdr",
        "titling", "sectsty", "titlesec", "booktabs", "longtable", "multirow",
        "wrapfig", "float", "colortbl", "pdflscape", "threeparttable",
        "xcolor", "graphicx", "setspace", "footmisc", "caption"
    )

    max_retries <- 3
    installed_all <- FALSE

    for (i in 1:max_retries) {
        message(sprintf("\nAttempt %d to install packages...", i))

        tryCatch(
            {
                # First try the standard way
                tinytex::tlmgr_install(required_pkgs)

                # Verify installation
                missing_pkgs <- setdiff(required_pkgs, tinytex::tl_pkgs())
                if (length(missing_pkgs) == 0) {
                    message("All required packages installed successfully.")
                    installed_all <- TRUE
                    break
                } else {
                    message("Missing packages: ", paste(missing_pkgs, collapse = ", "))
                }
            },
            error = function(e) {
                message("Package installation error: ", e$message)

                # Try alternative repository if standard fails
                if (i == 1) {
                    message("Trying alternative repository...")
                    tinytex::tlmgr_repo("https://mirror.ctan.org/systems/texlive/tlnet")
                }
            }
        )

        if (i < max_retries) Sys.sleep(5) # Wait before retry
    }

    if (!installed_all) {
        message("\nFailed to install all packages. You may need to:")
        message("1. Run this script as administrator")
        message("2. Check your internet connection")
        message("3. Try manual installation with:")
        message('   tinytex::tlmgr_install(c("geometry", "hyperref", ...))')
    }

    return(installed_all)
}

# Main installation process
message("Starting TinyTeX installation process...")

# Step 1: Install TinyTeX
if (install_tinytex_complete()) {
    # Step 2: Install packages
    message("\nInstalling required LaTeX packages...")
    install_required_packages()

    # Final check
    if (tinytex::is_tinytex()) {
        message("\nTinyTeX setup completed successfully!")
        message("Location: ", tinytex::tinytex_root())
    } else {
        message("\nInstallation completed but verification failed.")
        message("Try restarting R and running: tinytex::is_tinytex()")
    }
} else {
    message("\nTinyTeX installation failed. Possible solutions:")
    message("1. Run as administrator/root")
    message("2. Check antivirus/firewall settings")
    message("3. Try manual download from https://yihui.org/tinytex/")
}

# to install
# source("app/install_tinyx.R")
# source("install_tinyx.R")
# In Windows: R
# tinytex::use_tinytex()
# In Linux: R
# wget -qO- https://yihui.org/tinytex/install-bin-unix.sh | sh