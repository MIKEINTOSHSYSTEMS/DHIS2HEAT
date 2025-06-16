# Calculate weighted relative rank for ordered dimensions
calculate_relative_rank <- function(data) {
  if(!"population" %in% names(data) || !"subgroup_order" %in% names(data)) {
    return(NA)
  }
  
  data <- data[order(data$subgroup_order), ]
  cum_pop <- cumsum(data$population)
  total_pop <- sum(data$population)
  relative_rank <- (cum_pop - (data$population/2)) / total_pop
  
  return(relative_rank)
}

# Validate data for summary measures
validate_sm_data <- function(data) {
  required_cols <- c("estimate", "population", "dimension", "subgroup", "favourable_indicator")
  if (!all(required_cols %in% names(data))) {
    stop(paste(
      "Data missing required columns for summary measures calculation.",
      "Required columns:", paste(required_cols, collapse = ", ")
    ))
  }

  if (any(is.na(data$estimate))) {
    warning("Some estimates are NA - these will be excluded from calculations")
  }

  if (any(is.na(data$population))) {
    warning("Some population values are NA - these will be excluded from calculations")
  }

  if (length(unique(data$dimension)) > 1) {
    warning("Multiple dimensions detected - calculations will use all data")
  }

  if (length(unique(data$favourable_indicator)) > 1) {
    warning("Mixed favourable/adverse indicators detected - using first value")
  }
}

# Calculate confidence intervals with progress
calculate_sm_ci_with_progress <- function(measure, data, n_simulations = 100, session = NULL) {
  estimates <- numeric(n_simulations)

  withProgress(message = "Calculating confidence intervals", value = 0, {
    for (i in 1:n_simulations) {
      # Simulate data based on estimates and standard errors
      sim_data <- data.frame(
        estimate = rnorm(nrow(data), mean = data$estimate, sd = data$se),
        population = data$population
      )

      # Calculate measure for simulated data
      sim_results <- calculate_summary_measures(sim_data)
      estimates[i] <- sim_results[[measure]]

      # Update progress
      incProgress(1 / n_simulations, detail = paste("Simulation", i, "of", n_simulations))
    }
  })

  # Calculate confidence intervals
  ci <- quantile(estimates, probs = c(0.025, 0.975), na.rm = TRUE)
  return(ci)
}

# Format summary measure interpretation
format_interpretation <- function(measure, value, favourable) {
  interpretations <- list(
    difference = "Absolute difference between subgroups. Larger values indicate greater inequality.",
    ratio = "Relative ratio between subgroups. Values further from 1 indicate greater inequality.",
    aci = paste(
      "Measures absolute inequality across ordered subgroups.",
      ifelse(favourable,
        "Positive values indicate concentration among advantaged, negative among disadvantaged.",
        "Positive values indicate concentration among disadvantaged, negative among advantaged."
      )
    ),
    rci = paste(
      "Measures relative inequality across ordered subgroups.",
      ifelse(favourable,
        "Positive values indicate concentration among advantaged, negative among disadvantaged.",
        "Positive values indicate concentration among disadvantaged, negative among advantaged."
      )
    ),
    sii = paste(
      "Absolute difference between most and least advantaged, considering all subgroups.",
      ifelse(favourable,
        "Positive values indicate concentration among advantaged, negative among disadvantaged.",
        "Positive values indicate concentration among disadvantaged, negative among advantaged."
      )
    ),
    rii = paste(
      "Relative ratio between most and least advantaged, considering all subgroups.",
      "Values further from 1 indicate greater inequality."
    ),
    bgv = "Weighted average of squared differences from setting average. Larger values indicate greater inequality.",
    bgsd = "Square root of BGV - in same units as indicator. Larger values indicate greater inequality.",
    cov = "BGSD as percentage of setting average. Larger values indicate greater inequality.",
    mdmu = "Average absolute difference from setting average. Larger values indicate greater inequality.",
    mdmw = "Population-weighted average absolute difference from setting average. Larger values indicate greater inequality.",
    mdbu = "Average absolute difference from best performing subgroup. Larger values indicate greater inequality.",
    mdbw = "Population-weighted average absolute difference from best performing subgroup. Larger values indicate greater inequality.",
    mdru = "Average absolute difference from reference subgroup. Larger values indicate greater inequality.",
    mdrw = "Population-weighted average absolute difference from reference subgroup. Larger values indicate greater inequality.",
    idisu = "Average relative difference from setting average. Larger values indicate greater inequality.",
    idisw = "Population-weighted average relative difference from setting average. Larger values indicate greater inequality.",
    ti = "Measures inequality using logarithms of shares (sensitive to top differences). Larger values indicate greater inequality.",
    mld = "Measures inequality using logarithms of shares (sensitive to bottom differences). Larger values indicate greater inequality.",
    paf = paste(
      "Potential relative improvement if all subgroups reached reference level.",
      ifelse(favourable,
        "Positive values indicate potential gain.",
        "Negative values indicate potential gain."
      )
    ),
    par = paste(
      "Potential absolute improvement if all subgroups reached reference level.",
      ifelse(favourable,
        "Positive values indicate potential gain.",
        "Negative values indicate potential gain."
      )
    )
  )

  return(interpretations[[measure]])
}
