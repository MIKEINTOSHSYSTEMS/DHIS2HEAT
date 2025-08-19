# Load necessary libraries
library(httr)
library(jsonlite)
library(openxlsx)

# Set environment variables within R (replace with your actual credentials)
Sys.setenv(DHIS2_BASE_URL = "https://dhis.moh.gov.et")
Sys.setenv(DHIS2_USERNAME = "michaelk")
Sys.setenv(DHIS2_PASSWORD = "Dhis2_12345")

# Function to fetch data from DHIS2 API with error handling
get_dhis2_data <- function(endpoint) {
    cat("Starting to fetch data from DHIS2 API...\n")
    cat("Endpoint:", endpoint, "\n")

    dhis2_base_url <- Sys.getenv("DHIS2_BASE_URL")
    dhis2_username <- Sys.getenv("DHIS2_USERNAME")
    dhis2_password <- Sys.getenv("DHIS2_PASSWORD")

    url <- paste0(dhis2_base_url, endpoint)
    cat("Constructed URL:", url, "\n")

    response <- GET(url, authenticate(dhis2_username, dhis2_password))

    if (http_status(response)$category != "Success") {
        stop(paste(
            "Failed to retrieve data from", endpoint, ":",
            http_status(response)$message
        ))
    }

    cat("Response received. Status:", http_status(response)$category, "\n")

    # data <- fromJSON(content(response, as = "text"), flatten = TRUE) # OLDER VERSION OF JSONlite
    data <- fromJSON(content(response, as = "text"), simplifyDataFrame = TRUE)
    cat("Successfully fetched data from endpoint:", endpoint, "\n")

    return(data)
}

# Function to fetch analytics data for specific indicators, organisation units, and periods
fetch_indicator_data <- function(indicator_ids, org_unit_ids, periods, facility_type_ids = NULL, settlement_type_ids = NULL) {
    cat("Fetching analytics data...\n")

    dhis2_base_url <- Sys.getenv("DHIS2_BASE_URL")
    dhis2_username <- Sys.getenv("DHIS2_USERNAME")
    dhis2_password <- Sys.getenv("DHIS2_PASSWORD")

    # Combine parameters into API-friendly strings
    indicators_str <- paste(indicator_ids, collapse = ";")
    org_units_str <- paste(org_unit_ids, collapse = ";")
    periods_str <- paste(periods, collapse = ";")

    # Build endpoint
    endpoint <- paste0(
        "/api/analytics.json?dimension=dx:", indicators_str,
        "&dimension=ou:", org_units_str,
        "&dimension=pe:", periods_str
    )

    # Add facility type dimension if provided
    if (!is.null(facility_type_ids)) {
        facility_type_str <- paste(facility_type_ids, collapse = ";")
        endpoint <- paste0(endpoint, "&dimension=saIPeABoPMH:", facility_type_str)
    }

    # Add settlement type dimension if provided
    if (!is.null(settlement_type_ids)) {
        settlement_type_str <- paste(settlement_type_ids, collapse = ";")
        endpoint <- paste0(endpoint, "&dimension=hxLghWPCAXE:", settlement_type_str)
    }

    url <- paste0(dhis2_base_url, endpoint)
    cat("Constructed URL for Analytics Data Request:", url, "\n")

    response <- GET(url, authenticate(dhis2_username, dhis2_password))

    if (http_status(response)$category != "Success") {
        stop(paste("Failed to fetch data:", http_status(response)$message))
    }

    cat("Response received. Status:", http_status(response)$category, "\n")

    # data <- fromJSON(content(response, as = "text"), flatten = TRUE) # OLDER VERSION OF JSONlite
    data <- fromJSON(content(response, as = "text"), simplifyDataFrame = TRUE)
    cat("Successfully fetched data for indicators:", paste(indicator_ids, collapse = ", "), "\n")

    return(data)
}

# Function to fetch population data
fetch_population_data <- function(org_unit_ids, periods, zone_ids = NULL, woreda_ids = NULL) {
    cat("Fetching population data...\n")

    population_indicator <- "cpItyCYXKPd" # population indicator ID
    cat("Using population Indicator ID:", population_indicator, "\n")

    if (!is.null(zone_ids)) {
        population_data <- fetch_indicator_data(
            indicator_ids = population_indicator,
            org_unit_ids = zone_ids,
            periods = periods
        )
    } else if (!is.null(woreda_ids)) {
        population_data <- fetch_indicator_data(
            indicator_ids = population_indicator,
            org_unit_ids = woreda_ids,
            periods = periods
        )
    } else {
        population_data <- fetch_indicator_data(
            indicator_ids = population_indicator,
            org_unit_ids = org_unit_ids,
            periods = periods
        )
    }

    # Convert rows to data frame and assign column names
    population_rows <- as.data.frame(population_data$rows, stringsAsFactors = FALSE)
    colnames(population_rows) <- c("Indicator_ID", "Organisation_Unit_ID", "Period", "Population_Value")

    # Ensure Population_Value is numeric
    population_rows$Population_Value <- as.numeric(population_rows$Population_Value)

    # Handle missing population values (set to 0)
    population_rows$Population_Value[is.na(population_rows$Population_Value)] <- 0

    cat("Population data fetched successfully.\n")
    return(population_rows)
}

format_analytics_data <- function(analytics_data, indicators, org_units, population_data, indicator_map, dimension = "Region", settings_abbr = NULL, custom_scales = NULL) {
  cat("Formatting analytics data...\n")
  
  # Handle empty data case
  if (!"rows" %in% names(analytics_data)) {
    warning("No data returned from API.")
    return(data.frame())
  }
  
  # Check if headers exist
  if (!"headers" %in% names(analytics_data)) {
    stop("The 'headers' field is missing from the analytics data.")
  }
  
  headers <- analytics_data$headers
  n_headers <- nrow(headers)
  
  # Debug: Print headers structure
  cat("Headers structure:\n")
  str(headers)
  
  if (n_headers == 0) {
    warning("No headers found in the analytics data.")
    return(data.frame())
  }
  
  rows <- as.data.frame(analytics_data$rows, stringsAsFactors = FALSE)
  
  # Assign column names based on headers
  col_names <- headers$name
  colnames(rows) <- col_names
  
  # Debug: Print unique Indicator_IDs
  cat("Unique Indicator_IDs in data:", unique(rows$dx), "\n")
  
  # Ensure Indicator name is correctly mapped
  rows$indicator_name <- sapply(rows$dx, function(id) {
    indicator <- indicators[indicators$id == id, ]
    if (nrow(indicator) > 0) {
      return(indicator$displayName)
    } else if (!is.null(indicator_map[[id]])) {
      return(indicator_map[[id]]$name)
    } else {
      cat("Warning: No name found for Indicator_ID:", id, "\n")
      return(NA)
    }
  })
  
  rows$indicator_abbr <- sapply(rows$dx, function(id) {
    if (!is.null(indicator_map[[id]])) {
      return(indicator_map[[id]]$abbr)
    } else if (!is.null(settings_abbr) && id %in% names(settings_abbr)) {
      return(settings_abbr[[id]])
    } else {
      cat("Warning: No mapping found for Indicator_ID:", id, "\n")
      return(NA)
    }
  })
  
  # Handle Facility Type dimension
  if ("saIPeABoPMH" %in% colnames(rows)) {
    facility_type_map <- c(
      "kwcNbI9fPdB" = "Clinics",
      "j8SCxUTyzfm" = "Health Centers",
      "FW4oru60vgc" = "Health Post",
      "nVEDFMfnStv" = "Hospitals"
    )
    rows$subgroup <- facility_type_map[rows$saIPeABoPMH]
    rows$dimension <- "Facility Type"
    rows$saIPeABoPMH <- NULL
  } else if ("hxLghWPCAXE" %in% colnames(rows)) {
    settlement_type_map <- c(
      "nKT0uoFbxdf" = "Urban Settlement",
      "ZktuKijP5jN" = "Pastoral Settlement",
      "V9sleOboZJ1" = "Agrarian Settlement"
    )
    rows$subgroup <- settlement_type_map[rows$hxLghWPCAXE]
    rows$dimension <- "Settlement"
    rows$hxLghWPCAXE <- NULL
  } else {
    rows$dimension <- dimension
    
    # Set subgroup based on dimension using org_units parameter
    rows$subgroup <- sapply(rows$ou, function(id) {
      ou <- org_units[org_units$id == id, ]
      if (nrow(ou) > 0) {
        return(ou$displayName)
      } else {
        # Try to fetch from JSON if available
        json_path <- "./meta/all_organisationUnits.json"
        if (file.exists(json_path)) {
          tryCatch({
            org_units_json <- rjson::fromJSON(file = json_path)
            ou_meta <- Filter(function(x) x$id == id, org_units_json$organisationUnits)
            if (length(ou_meta) > 0) {
              return(ou_meta[[1]]$displayName %||% ou_meta[[1]]$name %||% id)
            }
          }, error = function(e) {
            cat("Warning: Failed to read org units JSON:", e$message, "\n")
          })
        }
        return(id) # Return ID as fallback
      }
    })
  }
  
  # Merge with population data
  if (!all(c("ou", "pe") %in% colnames(rows))) {
    stop("Required columns 'ou' or 'pe' missing from analytics data")
  }
  
  rows <- merge(rows, population_data, 
                by.x = c("ou", "pe"), 
                by.y = c("Organisation_Unit_ID", "Period"), 
                all.x = TRUE)
  
  if (!"Population_Value" %in% colnames(rows)) {
    warning("Population data not found in merged dataset")
    rows$population <- NA_real_
  } else {
    rows$population <- rows$Population_Value
  }
  
  # Ensure Value is numeric
  rows$value <- suppressWarnings(as.numeric(rows$value))
  if (any(is.na(rows$value))) {
    warning("Some values could not be converted to numeric")
  }
  
  # Initialize critical columns with proper length
  n <- nrow(rows)
  rows$favourable_indicator <- rep(0L, n)
  rows$ordered_dimension <- rep(0L, n)
  rows$subgroup_order <- rep(0L, n)
  rows$reference_subgroup <- rep(0L, n)
  
  # Calculate standard error with guard against edge cases
  value_capped <- pmin(pmax(rows$value, 0, na.rm = TRUE), 100)
  se_value <- (value_capped * (100 - value_capped)) / rows$population
  rows$se <- ifelse(se_value >= 0 & !is.na(se_value), sqrt(se_value), NA_real_)
  
  # Calculate confidence intervals
  rows$ci_lb <- rows$value - 1.96 * rows$se
  rows$ci_ub <- rows$value + 1.96 * rows$se
  
  # Cap confidence intervals at 0 and 100
  rows$ci_lb <- pmax(rows$ci_lb, 0, na.rm = TRUE)
  rows$ci_ub <- pmin(pmax(rows$ci_ub, 0, na.rm = TRUE), 100)
  
  # Set source metadata
  if (file.exists("saved_setting/source_settings.rds")) {
    source_settings <- readRDS("saved_setting/source_settings.rds")
    rows$setting <- source_settings$setting %||% "Ethiopia"
    rows$source <- source_settings$source %||% "DHIS_2"
    rows$iso3 <- source_settings$iso3 %||% "ETH"
  } else {
    rows$setting <- "Ethiopia"
    rows$source <- "DHIS_2"
    rows$iso3 <- "ETH"
  }
  
  rows$date <- as.integer(rows$pe)
  rows$estimate <- pmin(pmax(rows$value, 0, na.rm = TRUE), 100) / 100
  rows$note <- ""
  
  # Calculate setting average
  rows$setting_average <- ave(rows$estimate, 
                             rows$indicator_name, 
                             rows$source, 
                             rows$date, 
                             FUN = function(x) mean(x, na.rm = TRUE))
  
  # Format setting average
  rows$setting_average <- ifelse(is.na(rows$setting_average),
                                "0.00%",
                                sprintf("%.2f%%", rows$setting_average * 100))
  
  # Handle ordered settings if available
  if (file.exists("saved_setting/ordered_settings.rds")) {
    ordered_settings <- readRDS("saved_setting/ordered_settings.rds")
    
    for (ind in names(ordered_settings)) {
      if (ind %in% rows$indicator_name) {
        setting <- ordered_settings[[ind]]
        idx <- which(rows$indicator_name == ind & rows$dimension == setting$dimension)
        
        if (length(idx) > 0) {
          # Mark ordered dimension
          rows$ordered_dimension[idx] <- 1L
          
          # Set reference subgroup
          rows$reference_subgroup[idx] <- as.integer(rows$subgroup[idx] == setting$reference)
          
          # Set subgroup order if dimension is Region/Zone/Woreda
          if (setting$dimension %in% c("Region", "Zone", "Woreda")) {
            subgroups <- unique(rows$subgroup[idx])
            if (length(subgroups) > 0) {
              subgroup_order <- setNames(seq_along(subgroups), subgroups)
              rows$subgroup_order[idx] <- subgroup_order[rows$subgroup[idx]]
            }
          }
        }
      }
    }
  }
  
  # Set indicator scale (default 100)
  rows$indicator_scale <- rep(100L, n)
  if (!is.null(custom_scales)) {
    for (id in names(custom_scales)) {
      rows$indicator_scale[rows$dx == id] <- as.integer(custom_scales[[id]])
    }
  }
  
  # Define required output columns
  required_columns <- c(
    "setting", "date", "source", "indicator_abbr", "indicator_name",
    "dimension", "subgroup", "estimate", "se", "ci_lb", "ci_ub",
    "population", "note", "setting_average", "iso3", "favourable_indicator",
    "indicator_scale", "ordered_dimension", "subgroup_order", "reference_subgroup"
  )
  
  # Check for missing columns
  missing_cols <- setdiff(required_columns, names(rows))
  if (length(missing_cols) > 0) {
    warning("Creating missing columns: ", paste(missing_cols, collapse = ", "))
    for (col in missing_cols) {
      rows[[col]] <- if (col == "note") "" else NA
    }
  }
  
  # Select and order columns
  formatted_rows <- rows[, required_columns, drop = FALSE]
  
  # Sort by indicator_name and date
  formatted_rows <- formatted_rows[order(formatted_rows$indicator_name, formatted_rows$date), ]
  
  # Replace NA values with appropriate defaults
  numeric_cols <- c("estimate", "se", "ci_lb", "ci_ub", "population", 
                   "favourable_indicator", "indicator_scale", 
                   "ordered_dimension", "subgroup_order", "reference_subgroup")
  
  for (col in numeric_cols) {
    if (col %in% names(formatted_rows)) {
      formatted_rows[[col]][is.na(formatted_rows[[col]])] <- 0
    }
  }
  
  cat("Formatting complete. Returning", nrow(formatted_rows), "rows.\n")
  return(formatted_rows)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x


# Indicator mapping for names and abbreviations
# Test Indicators
# indicator_map <- list(
#    "AxBNUGGwwXJ" = list("name" = "Skilled birth attendants", "abbr" = "Sba"),
#    "qbwqrHsRlr4" = list("name" = "Ratio of hospitals per populations", "abbr" = "RHPP")
# )

indicator_map <- list(
  "wiCvecPtl0T" = list("name" = "Neonates discharged as dead from NICU", "abbr" = "3Pndn"),
  "BsB9R6pi0uf" = list("name" = "Neonates discharged as recovered from NICU", "abbr" = "1Pnrn"),
  "SUYR67uPFMk" = list("name" = "Neonates transferred out from NICU", "abbr" = "2Pnton"),
  "DJiHqs9eJoN" = list("name" = "Other NICU discharges", "abbr" = "Pndno"),
  "yu81UlnsaiH" = list("name" = "Full Immunization Coverage (< 1 year)", "abbr" = "Fv"),
  "r2eJDQLBSY0" = list("name" = "Pentavalent 1st Dose Coverage (< 1 year)", "abbr" = "1Pv1"),
  "YPfTaU4EtKm" = list("name" = "Pentavalent 3rd Dose Coverage (< 1 year)", "abbr" = "3Pv3"),

 # Categorized By SEX 
  # "Fb2ajzvMxH4" = list("name" = "HHs enrolled in CBHI (Woreda)", "abbr" = "PCBHI_member_enr_1"),
  # "M5KYiOXZkr7" = list("name" = "Functional General Hospital to Population Ratio", "abbr" = "RHPP_1"),
  # "AYQLnL5gKNe" = list("name" = "Functional Primary Hospital to Population Ratio", "abbr" = "RHPP_2"),
  # "tFX3fkTo0tW" = list("name" = "Functional Referral Hospital to Population Ratio", "abbr" = "RHPP_3"),
  # "Fpxqdh9XxGd" = list("name" = "Adults on ART aged 15-19 by regimen", "abbr" = "CART_1"),
  # "TYYcdumng5v" = list("name" = "Adults on ART aged 15-19 by Sex", "abbr" = "CART_2"),
  # "zBtEVn0BWes" = list("name" = "Adults on ART aged 15-19 by Sex", "abbr" = "CART_3"),
  # "qiO8zxIsdVf" = list("name" = "Adults on ART aged 20-24 by Sex", "abbr" = "CART_4"),
  # "NMzj2KrZNpD" = list("name" = "Adults on ART aged 25-29 by Sex", "abbr" = "CART_5"),
  # "FDbjgpb71v6" = list("name" = "Adults on ART aged 25-49 by Sex", "abbr" = "CART_6"),
  # "gecQJ4OuMkd" = list("name" = "Adults on ART aged 30-34 by Sex", "abbr" = "CART_7"),
  # "FhvlCdtUnrR" = list("name" = "Adults on ART aged 35-39 by Sex", "abbr" = "CART_8"),
  # "lp1lqrdAlBh" = list("name" = "Adults on ART aged 40-44 by Sex", "abbr" = "CART_9"),
  # "niTCBDGkWru" = list("name" = "Adults on ART aged 45-49 by Sex", "abbr" = "CART_10"),
  # "H010uaIIg62" = list("name" = "Adults on ART aged 50+ by Sex", "abbr" = "CART_11"),

  "K4xjRgFlHVA" = list("name" = "Children on ART aged 10-14 by Sex", "abbr" = "CART_16"),
  "STKCPnfJE0J" = list("name" = "Children on ART aged 1-4 by Sex", "abbr" = "CART_18"),
  "DN4DxOG0KcJ" = list("name" = "Children on ART aged <1 by Sex", "abbr" = "CART_20"),
  "lyvLEgMy53I" = list("name" = "Children on ART aged 5-9 by sex", "abbr" = "CART_21"),

# Categorized by Regimen
  "erfov4UPPwm" = list("name" = "Children on ART aged 10-14 by regimen", "abbr" = "CART_15"),
  "T2aCB3BdjFo" = list("name" = "Children on ART aged 1-4 by regimen", "abbr" = "CART_17"),
  "moCGWgByYvu" = list("name" = "Children on ART aged <1 by regimen", "abbr" = "CART_19"),
  "DOjmzNXoBUS" = list("name" = "Adults on ART >=20 by regimen", "abbr" = "CART_12"),
  "YyB2gQ7FNkU" = list("name" = "Children on ART aged 5-9 by regimen", "abbr" = "CART_22"),
  "EM02m2HPZG6" = list("name" = "Total on ART by age, sex, regimen", "abbr" = "CART_27"),
  "hE8RPX6Vaib" = list("name" = "Children on ART (<19) by regimen", "abbr" = "CART_28"),  

# No Categorization  
  "QPnMWX0VHL5" = list("name" = "Adults on ART", "abbr" = "CART_13"),
  "cpVK4vg0qP9" = list("name" = "Children on ART", "abbr" = "CART_14"),
  "ndqrcnzGcUU" = list("name" = "Under 15 female on ART", "abbr" = "CART_23"),
  "cMvVX98HjUA" = list("name" = "Under 15 male on ART", "abbr" = "CART_24"),
  "YWkcclXH1R5" = list("name" = "Female >15 years on ART", "abbr" = "CART_25"),
  "nIKDxJxGgxL" = list("name" = "Male >15 years on ART", "abbr" = "CART_26"),
  "myWEyjn5FLo" = list("name" = "Leprosy Grade II Disability Rate", "abbr" = "Grade2DR"),
  "n0lUZ6iJt7V" = list("name" = "Female Leadership in Health Facilities", "abbr" = "LP"),
  "Lf9u3N1BPO2" = list("name" = "Primary Health Care Facilities with CSC >=80%", "abbr" = "PHCUsCS"),
  "NLMn0xklEBd" = list("name" = "IRS Coverage in Unit Structures", "abbr" = "IRS"),
  "kdkf3Zb6VD3" = list("name" = "ANC 4+ Contacts Coverage", "abbr" = "Anc4"),
  "iMMy5SY9mnx" = list("name" = "C-Section Rate", "abbr" = "Cs"),
  "EmPOBxh5K5S" = list("name" = "Early PNC Coverage", "abbr" = "Pnc_1"),
  "kiWHNOMaX8W" = list("name" = "Early PNC Coverage (0-2 days)", "abbr" = "Pnc_2"),
  "qVuwmOehUwg" = list("name" = "Early PNC Coverage (2-3 days)", "abbr" = "Pnc_3"),
  "pz1GAp7xV2I" = list("name" = "Early PNC Coverage (4-7 days)", "abbr" = "Pnc_4"),
  "AxBNUGGwwXJ" = list("name" = "% of Births Attended by Skilled Personnel", "abbr" = "Sba"),
  "dAGUPaBm989" = list("name" = "Assistive Technology Service Coverage", "abbr" = "ATSU"),
  "oksEqSjFgQm" = list("name" = "Hospital Bed Density", "abbr" = "HBD"),
  "QmJZDJx2vz5" = list("name" = "Inpatient Mortality Rate", "abbr" = "IPMR"),
  "TBAxe5hsc55" = list("name" = "Outpatient Attendance per Capita", "abbr" = "OPD_A"),
  "nY0Smor9ccz" = list("name" = "% of HEI with Virological Test", "abbr" = "PHIVexposedVT"),
  "x2urWGBth5P" = list("name" = "Women Aged 30–49 Screened for Cervical Cancer", "abbr" = "ScrCervCa"),
  "yzeUuGbwEqb" = list("name" = "HIV Positive Adults and Children on ART", "abbr" = "CART_29"),
  "QquJVpoNpH2" = list("name" = "TB Treatment Coverage", "abbr" = "TBRxCov")
)

# Fetch metadata
cat("Fetching metadata...\n")
indicators_metadata <- get_dhis2_data("/api/indicators?paging=false")$indicators
org_units_metadata <- get_dhis2_data("/api/organisationUnits?paging=false")$organisationUnits

# Fetch Zones and Woredas metadata
zones_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=3&paging=false")$organisationUnits
woredas_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=4&paging=true&pageSize=600")$organisationUnits

cat("Fetched metadata. Indicators:", length(indicators_metadata), "Organisation Units:", length(org_units_metadata), "\n")
cat("Zones:", length(zones_metadata), "Woredas:", length(woredas_metadata), "\n")

# Define specific indicators, organisation units, and periods
# Testing
# specific_indicators <- c("AxBNUGGwwXJ", "qbwqrHsRlr4")

# Main Indicators

specific_indicators <- c(
    "wiCvecPtl0T", "BsB9R6pi0uf", "SUYR67uPFMk", "DJiHqs9eJoN", "yu81UlnsaiH",
    "r2eJDQLBSY0", "YPfTaU4EtKm",
    #  "Fb2ajzvMxH4", "M5KYiOXZkr7", "AYQLnL5gKNe","tFX3fkTo0tW",
    #    "Fpxqdh9XxGd", "TYYcdumng5v", "zBtEVn0BWes", "qiO8zxIsdVf",
    #    "NMzj2KrZNpD", "FDbjgpb71v6", "gecQJ4OuMkd", "FhvlCdtUnrR", "lp1lqrdAlBh",
    #    "niTCBDGkWru", "H010uaIIg62",
    "DOjmzNXoBUS", "QPnMWX0VHL5", "cpVK4vg0qP9",
    "erfov4UPPwm", "K4xjRgFlHVA", "T2aCB3BdjFo", "STKCPnfJE0J", "moCGWgByYvu",
    "DN4DxOG0KcJ", "lyvLEgMy53I", "YyB2gQ7FNkU", "ndqrcnzGcUU", "cMvVX98HjUA",
    "YWkcclXH1R5", "nIKDxJxGgxL", "EM02m2HPZG6", "hE8RPX6Vaib", "myWEyjn5FLo",
    "n0lUZ6iJt7V", "Lf9u3N1BPO2", "NLMn0xklEBd", "kdkf3Zb6VD3", "iMMy5SY9mnx",
    "EmPOBxh5K5S", "kiWHNOMaX8W", "qVuwmOehUwg", "pz1GAp7xV2I", "AxBNUGGwwXJ",
    "dAGUPaBm989", "oksEqSjFgQm", "QmJZDJx2vz5", "TBAxe5hsc55", "nY0Smor9ccz",
    "x2urWGBth5P", "yzeUuGbwEqb", "QquJVpoNpH2"
)


# Test Regions
specific_org_units <- c(
    "yY9BLUUegel", "UFtGyqJMEZh", "yb9NKGA8uqt", "Fccw8uMlJHN",
    "tDoLtk2ylu4", "G9hDiPNoB7d", "moBiwh9h5Ce", "b9nYedsL8te",
    "XU2wpLlX4Vk", "xNUoZIrGKxQ", "PCKGSJoNHXi", "a2QIIR2UXcd",
    "HIlnt7Qj8do", "Gmw0DJLXGtx"
)

# Main Regions
# specific_org_units <- c(
#    "yY9BLUUegel", "UFtGyqJMEZh", "yb9NKGA8uqt", "Fccw8uMlJHN",
#    "tDoLtk2ylu4", "G9hDiPNoB7d", "moBiwh9h5Ce", "b9nYedsL8te",
#    "XU2wpLlX4Vk", "xNUoZIrGKxQ", "PCKGSJoNHXi", "a2QIIR2UXcd",
#    "HIlnt7Qj8do", "Gmw0DJLXGtx"
# )

# testing periods
periods <- c("2015", "2016", "2017")

# Main Periods
# periods <- c("2013", "2014", "2015", "2016", "2017")

cat("Specific Indicators:", paste(specific_indicators, collapse = ", "), "\n")
cat("Specific Organisation Units:", paste(specific_org_units, collapse = ", "), "\n")
cat("Periods:", paste(periods, collapse = ", "), "\n")

# Fetch population data for Region
population_data_region <- fetch_population_data(specific_org_units, periods)

# Process data for Region
analytics_data_region <- fetch_indicator_data(specific_indicators, specific_org_units, periods)
formatted_data_region <- format_analytics_data(analytics_data_region, indicators_metadata, org_units_metadata, population_data_region, indicator_map, dimension = "Region")

# Process data for Zone
zone_data <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=3&paging=false")
specific_zones <- zone_data$organisationUnits$id
population_data_zone <- fetch_population_data(NULL, periods, zone_ids = specific_zones) # Pass Zone IDs for population data
analytics_data_zone <- fetch_indicator_data(specific_indicators, specific_zones, periods)
formatted_data_zone <- format_analytics_data(analytics_data_zone, indicators_metadata, org_units_metadata, population_data_zone, indicator_map, dimension = "Zone")

# Fetch Woreda data (first 600 & or 200)
woreda_data <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=4&paging=true&pageSize=600")
specific_woredas <- woreda_data$organisationUnits$id
population_data_woreda <- fetch_population_data(NULL, periods, woreda_ids = specific_woredas) # Pass Woreda IDs for population data
analytics_data_woreda <- fetch_indicator_data(specific_indicators, specific_woredas, periods)
formatted_data_woreda <- format_analytics_data(analytics_data_woreda, indicators_metadata, org_units_metadata, population_data_woreda, indicator_map, dimension = "Woreda")


# Process data for Facility Type
facility_type_ids <- c("kwcNbI9fPdB", "j8SCxUTyzfm", "FW4oru60vgc", "nVEDFMfnStv")
analytics_data_facility <- fetch_indicator_data(
    specific_indicators,
    specific_org_units, # Use appropriate org units (e.g., national level)
    periods,
    facility_type_ids = facility_type_ids
)
population_data_facility <- fetch_population_data(specific_org_units, periods)
formatted_data_facility <- format_analytics_data(
    analytics_data_facility,
    indicators_metadata,
    org_units_metadata,
    population_data_facility,
    indicator_map,
    dimension = "Facility Type"
)

# Process data for Settlement Type
settlement_type_ids <- c("nKT0uoFbxdf", "ZktuKijP5jN", "V9sleOboZJ1")
analytics_data_settlement <- fetch_indicator_data(
    specific_indicators,
    specific_org_units, # Use appropriate org units (e.g., national level)
    periods,
    settlement_type_ids = settlement_type_ids
)
population_data_settlement <- fetch_population_data(specific_org_units, periods)
# population_data_settlement <- fetch_population_data(specific_zones, periods)  # Null Values for pop
# population_data_settlement <- fetch_population_data(specific_woredas, periods) # Null Values for pop
formatted_data_settlement <- format_analytics_data(
    analytics_data_settlement,
    indicators_metadata,
    org_units_metadata,
    population_data_settlement, # Or appropriate population data
    indicator_map,
    dimension = "Settlement"
)

# Combine all data
combined_data <- rbind(formatted_data_region, formatted_data_zone, formatted_data_woreda, formatted_data_facility, formatted_data_settlement)

# Combine and write to Excel
# combined_data <- rbind(formatted_data_region, formatted_data_zone, formatted_data_woreda)

if (nrow(combined_data) > 0) {
    cat("Writing combined data to Excel...\n")
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_name <- paste0("fetched_data/xlsx/DHIS2_DATA_", timestamp, ".xlsx")
    # file_name <- paste0("/srv/shiny-server/fetched_data/xlsx/DHIS2_DATA_", timestamp, ".xlsx")
    write.xlsx(combined_data,
        file = file_name,
        sheetName = "Indicators_Data", rowNames = FALSE
    )
    cat("Excel file '", file_name, "' created successfully.\n")
} else {
    cat("No data to write.\n")
}