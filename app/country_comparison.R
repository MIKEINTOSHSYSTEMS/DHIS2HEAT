# country_comparison.R

# Function to load country comparison data
load_country_comparison <- function(countries, start_year, end_year) {
    # Implement your data loading logic here
    # This should return a dataframe with country comparison data
}

# Function to validate country selections
validate_country_selection <- function(selected_countries) {
    if (length(selected_countries) < 2) {
        stop("Please select at least 2 countries for comparison")
    }
}