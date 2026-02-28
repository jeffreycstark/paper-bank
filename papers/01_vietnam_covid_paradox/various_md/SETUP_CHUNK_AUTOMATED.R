# ============================================================================
# IMPROVED SETUP CHUNK - Copy this into your manuscript.qmd
# ============================================================================
# Replace your current setup chunk with this version
# This adds automated value extraction for inline use throughout the manuscript
# ============================================================================

```{r setup, include=FALSE}
# ============================================================================
# MANUSCRIPT SETUP: Load Pre-Computed Analysis Results
# ============================================================================

# Load packages
library(tidyverse)
library(here)
library(gt)
library(gtExtras)
library(modelsummary)
library(knitr)

# Set global options
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 6,
  dpi = 300
)

# Set theme for plots
theme_set(theme_minimal(base_size = 12))

# Define results directory path
results_dir <- file.path("..", "analysis", "results")

# ============================================================================
# FUNCTION: Safely load RDS files
# ============================================================================
safe_load <- function(filename) {
  filepath <- file.path(results_dir, filename)
  if (file.exists(filepath)) {
    readRDS(filepath)
  } else {
    NULL
  }
}

# ============================================================================
# LOAD ALL ANALYSIS RESULTS
# ============================================================================

# Descriptive results
paradox_summary <- safe_load("descriptive_paradox_summary.rds")
descriptive_stats <- safe_load("descriptive_sample_characteristics.rds")
scale_reliability <- safe_load("descriptive_scale_reliability.rds")
cor_matrix <- safe_load("descriptive_correlation_matrix.rds")
bootstrap_cors <- safe_load("descriptive_bootstrap_correlations.rds")
effect_sizes <- safe_load("descriptive_effect_sizes.rds")
economic_impacts <- safe_load("descriptive_economic_impacts.rds")

# Hypothesis test results
h1_results <- safe_load("h1_infection_effects.rds")
h1c_model <- safe_load("h1c_interaction_model.rds")
h2a_results <- safe_load("h2a_trust_direct_effects.rds")
h2b_results <- safe_load("h2b_mediation_paths.rds")
h3a_results <- safe_load("h3a_democracy_effects.rds")
h3b_results <- safe_load("h3b_authoritarianism_effects.rds")
h4b_results <- safe_load("h4b_trust_vs_economic.rds")
models_core <- safe_load("m1_core_models.rds")
models_full <- safe_load("m2_full_models.rds")
hypothesis_summary <- safe_load("hypothesis_summary.rds")

# Mediation results
mediation_paths <- safe_load("med_path_coefficients.rds")
mediation_bootstrap <- safe_load("med_bootstrap_results.rds")
mediation_summary <- safe_load("med_summary.rds")
mediation_sensitivity <- safe_load("med_sensitivity.rds")
mediation_low_auth <- safe_load("med_low_auth.rds")
mediation_high_auth <- safe_load("med_high_auth.rds")
mediation_countries <- safe_load("med_cross_country.rds")

# Country comparison results
vietnam_correlations <- safe_load("country_vietnam_correlations.rds")
cambodia_correlations <- safe_load("country_cambodia_correlations.rds")
thailand_correlations <- safe_load("country_thailand_correlations.rds")
fisher_infection_approval <- safe_load("country_fisher_infection_approval.rds")
fisher_trust_approval <- safe_load("country_fisher_trust_approval.rds")
prop_mediated <- safe_load("country_proportion_mediated.rds")
pub_summary_table <- safe_load("country_summary_table.rds")

# Robustness check results
missing_data_summary <- safe_load("robust_missing_summary.rds")
ordinal_vs_ols <- safe_load("robust_ordinal_vs_ols.rds")
robust_comparison <- safe_load("robust_regression_comparison.rds")
quantile_comparison <- safe_load("robust_quantile_regression.rds")
outlier_diagnostics <- safe_load("robust_outlier_diagnostics.rds")
outlier_sensitivity <- safe_load("robust_excluding_outliers.rds")
complete_case_test <- safe_load("robust_complete_case_test.rds")
robustness_summary <- safe_load("robust_all_checks_summary.rds")

# ============================================================================
# NEW: EXTRACT KEY VALUES FOR INLINE USE THROUGHOUT MANUSCRIPT
# ============================================================================

# Helper function to safely extract values with fallback
get_value <- function(data, country, column, fallback) {
  if (is.null(data)) return(fallback)
  value <- data %>% filter(country_name == {{country}}) %>% pull({{column}})
  if (length(value) == 0 || is.na(value)) return(fallback)
  return(value[1])
}

# VIETNAM VALUES (with fallbacks)
vietnam <- list(
  n = get_value(paradox_summary, "Vietnam", n, 1237),
  infection_pct = get_value(paradox_summary, "Vietnam", infection_rate, 0.659) * 100,
  approval_pct = get_value(paradox_summary, "Vietnam", approval_rate, 97.5),
  trust_pct = get_value(paradox_summary, "Vietnam", trust_covid_rate, 91.9)
)

# CAMBODIA VALUES (with fallbacks)
cambodia <- list(
  n = get_value(paradox_summary, "Cambodia", n, 1242),
  infection_pct = get_value(paradox_summary, "Cambodia", infection_rate, 0.087) * 100,
  approval_pct = get_value(paradox_summary, "Cambodia", approval_rate, 93.6),
  trust_pct = get_value(paradox_summary, "Cambodia", trust_covid_rate, 89.2)
)

# THAILAND VALUES (with fallbacks)
thailand <- list(
  n = get_value(paradox_summary, "Thailand", n, 1200),
  infection_pct = get_value(paradox_summary, "Thailand", infection_rate, 0.401) * 100,
  approval_pct = get_value(paradox_summary, "Thailand", approval_rate, 37.7),
  trust_pct = get_value(paradox_summary, "Thailand", trust_covid_rate, 34.0)
)

# TOTAL SAMPLE SIZE
total_n <- if (!is.null(paradox_summary)) sum(paradox_summary$n, na.rm = TRUE) else 3679

# CALCULATED RATIOS (for narrative comparisons)
vietnam_vs_cambodia_infection_ratio <- vietnam$infection_pct / cambodia$infection_pct

# ============================================================================
# HELPER FUNCTIONS FOR CLEANER MANUSCRIPT CODE
# ============================================================================

# Format correlation with significance stars
format_cor <- function(cor_value, p_value) {
  stars <- if (p_value < 0.001) "***" else if (p_value < 0.01) "**" else if (p_value < 0.05) "*" else ""
  paste0("r = ", round(cor_value, 3), stars)
}

# Create correlation table from results list
create_cor_table <- function(results_list) {
  if (is.null(results_list)) {
    return(tibble(Note = "Data not available. Render analysis scripts first."))
  }
  
  tibble(
    Country = names(results_list),
    Correlation = sapply(results_list, function(x) x$coef),
    `p-value` = sapply(results_list, function(x) x$p),
    `95% CI Lower` = sapply(results_list, function(x) x$ci_lower),
    `95% CI Upper` = sapply(results_list, function(x) x$ci_upper),
    N = sapply(results_list, function(x) x$n)
  )
}

# Create placeholder table when data not available
create_placeholder <- function(table_name, script_to_run) {
  tibble(
    Status = paste("Missing:", table_name),
    Action = paste("Render:", script_to_run)
  ) %>%
    gt() %>%
    tab_header(title = "Data Not Yet Available") %>%
    tab_style(
      style = cell_fill(color = "#fff3cd"),
      locations = cells_body()
    )
}

# ============================================================================
# CONFIRMATION MESSAGE
# ============================================================================

loaded_count <- sum(!sapply(mget(ls(pattern = "^(paradox_|descriptive_|h[1-4]|models_|hypothesis_|mediation_|vietnam_|cambodia_|thailand_|fisher_|prop_|pub_|missing_|ordinal_|robust_|quantile_|outlier_|complete_|robustness_)")), is.null))

cat("✓ Loaded", loaded_count, "pre-computed analysis result objects\n")
cat("✓ Automated values for inline use:\n")
cat("  Vietnam: n =", vietnam$n, ", infection =", round(vietnam$infection_pct, 1), "%, approval =", round(vietnam$approval_pct, 1), "%\n")
cat("  Cambodia: n =", cambodia$n, ", infection =", round(cambodia$infection_pct, 1), "%, approval =", round(cambodia$approval_pct, 1), "%\n")
cat("  Thailand: n =", thailand$n, ", infection =", round(thailand$infection_pct, 1), "%, approval =", round(thailand$approval_pct, 1), "%\n")
cat("  Total N =", format(total_n, big.mark = ","), "\n")
```
