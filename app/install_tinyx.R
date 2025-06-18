# install_tinytex.R - Complete TinyTeX installation script

# Function to check if a command exists (useful for general system checks, less critical for TinyTeX-specific logic)
command_exists <- function(cmd) {
    Sys.which(cmd) != ""
}

# Install TinyTeX with robust error handling
install_tinytex_complete <- function() {
    # Check if TinyTeX R package is installed
    if (!requireNamespace("tinytex", quietly = TRUE)) {
        message("Installing tinytex R package...")
        install.packages("tinytex", repos = "https://cloud.r-project.org/")
        # After installation, load the package
        if (!requireNamespace("tinytex", quietly = TRUE)) {
            message("Failed to install 'tinytex' R package. Cannot proceed.")
            return(FALSE)
        }
    }

    # Check if TinyTeX distribution is already installed
    if (tinytex::is_tinytex()) {
        message("TinyTeX distribution is already installed.")
        # Ensure it's callable right after this check, crucial for some environments
        if (!tinytex::tlmgr_path() %in% Sys.getenv("PATH")) {
            message("TinyTeX is installed but its path might not be correctly set. Attempting to fix.")
            tryCatch({
                tinytex::use_tinytex() # This helps ensure PATH is set for the current session
                if (!tinytex::is_tinytex()) stop("Failed to make TinyTeX callable.")
                message("TinyTeX path fixed for current session.")
            }, error = function(e) {
                message("Could not fix TinyTeX path: ", e$message, " - a restart might be required.")
            })
        }
        return(TRUE)
    }

    message("Installing TinyTeX distribution...")

    # Try different installation methods
    tryCatch(
        {
            # Method 1: Standard installation via tinytex R package
            message("Trying standard TinyTeX installation (tinytex::install_tinytex())...")
            tinytex::install_tinytex()

            # Verify installation immediately
            if (!tinytex::is_tinytex()) {
                stop("Standard installation verification failed: tlmgr not found after install.")
            }

            message("TinyTeX installed successfully via standard method.")
            return(TRUE)
        },
        error = function(e) {
            message("Standard installation failed: ", e$message)
            message("Trying alternative installation method...")

            # Method 2: Alternative manual installation based on OS
            tryCatch(
                {
                    if (.Platform$OS.type == "windows") {
                        message("Attempting manual download and unzip for Windows...")
                        temp_zip <- tempfile(fileext = ".zip")
                        utils::download.file(
                            "https://yihui.org/tinytex/TinyTeX-1.zip",
                            destfile = temp_zip, mode = "wb"
                        )
                        utils::unzip(temp_zip, exdir = tempdir())
                        tinytex_root <- file.path(tempdir(), "TinyTeX")
                        # You might need to adjust this if tinytex::use_tinytex()
                        # expects a different structure or needs to copy the files.
                        # The best practice is usually to let install_tinytex handle it.
                        # For a truly manual unzip, you'd need to set PATH manually or copy.
                        # For simplicity, relying on use_tinytex is preferred.
                        tinytex::use_tinytex(dir = tinytex_root) # Point tinytex R package to the unzipped location
                        unlink(temp_zip) # Clean up
                    }
                    else { # For Unix-like systems (Linux, macOS)
                        message("Attempting manual shell script download for Unix-like system...")
                        # This command installs TinyTeX to ~/.TinyTeX
                        system("wget -qO- https://yihui.org/tinytex/install-bin-unix.sh | sh")
                        # After manual install, tell tinytex R package where to find it
                        tinytex::use_tinytex(dir = file.path(Sys.getenv("HOME"), ".TinyTeX"))
                    }

                    # Verify alternative installation
                    if (!tinytex::is_tinytex()) {
                        stop("Alternative installation verification failed: tlmgr not found.")
                    }

                    message("TinyTeX installed successfully via alternative method.")
                    return(TRUE)
                },
                error = function(e2) {
                    message("Alternative installation also failed: ", e2$message)
                    message("Manual intervention may be required.")
                    return(FALSE)
                }
            )
        }
    )
}

# Install required LaTeX packages (now focusing on collection-latexextra)
install_required_latex_packages <- function() {
    # Set a reliable repository for consistency
    message("Setting TinyTeX repository to: https://mirror.ctan.org/systems/texlive/tlnet")
    tinytex::tlmgr_repo("https://mirror.ctan.org/systems/texlive/tlnet")

    package_to_install <- "collection-latexextra"
    max_retries <- 3
    installed_successfully <- FALSE

    for (i in 1:max_retries) {
        message(sprintf("\nAttempt %d to install LaTeX bundle '%s'...", i, package_to_install))

        tryCatch(
            {
                # Check if the bundle is already installed
                if (package_to_install %in% tinytex::tl_pkgs()) {
                    message(sprintf("LaTeX bundle '%s' is already installed.", package_to_install))
                    installed_successfully <- TRUE
                    break
                }

                # Attempt to install the bundle
                tinytex::tlmgr_install(package_to_install)

                # Verify installation
                if (package_to_install %in% tinytex::tl_pkgs()) {
                    message(sprintf("LaTeX bundle '%s' installed successfully.", package_to_install))
                    installed_successfully <- TRUE
                    break
                } else {
                    stop(sprintf("Verification failed for '%s' after installation attempt.", package_to_install))
                }
            },
            error = function(e) {
                message(sprintf("Installation attempt for '%s' failed: %s", package_to_install, e$message))
                if (i < max_retries) {
                    message("Retrying in 5 seconds...")
                    Sys.sleep(5) # Wait before retry
                }
            }
        )
    }

    if (!installed_successfully) {
        message(sprintf("\nFailed to install LaTeX bundle '%s' after %d attempts.", package_to_install, max_retries))
        message("You may need to manually install it:")
        message(sprintf("  tinytex::tlmgr_install(\"%s\")", package_to_install))
    }
    return(installed_successfully)
}

# --- Main installation process ---
message("Starting TinyTeX setup script...")

# Step 1: Install or ensure TinyTeX distribution is present
if (install_tinytex_complete()) {
    # Step 2: Install required LaTeX packages
    message("\nProceeding to install core LaTeX packages...")
    if (install_required_latex_packages()) {
        message("\nTinyTeX and all required LaTeX packages are now set up!")
        message("TinyTeX Root: ", tinytex::tinytex_root())
    } else {
        message("\nWARNING: TinyTeX is installed, but some LaTeX packages could not be installed.")
        message("PDF generation might still encounter missing package errors.")
    }
} else {
    message("\nFATAL ERROR: TinyTeX distribution could not be installed.")
    message("PDF generation will not be possible without a working TinyTeX installation.")
    message("Please check the messages above for hints or try manual installation as described in TinyTeX documentation.")
}
# This script is designed to install TinyTeX and required LaTeX packages
# to install
# source("app/install_tinyx.R")
# source("install_tinyx.R")
# In Windows: R
# tinytex::use_tinytex()
# In Linux: R
# wget -qO- https://yihui.org/tinytex/install-bin-unix.sh | sh