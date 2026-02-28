# =============================================================================
# Save Pooled Descriptive Statistics for Manuscript
# =============================================================================
# Purpose: Calculate and save pooled (overall) descriptive statistics
# to avoid recalculating in manuscript.qmd
# Created: 2026-01-06

library(tidyverse)
library(here)

# Load data
ab_data <- readRDS(here("data", "processed", "ab_analysis_v2.rds"))

# Calculate pooled statistics (same as manuscript.qmd lines 308-343)
pooled_descriptives <- list(
  # DV: Government approval
  approval_mean = mean(ab_data$covid_govt_handling, na.rm = TRUE),
  approval_sd = sd(ab_data$covid_govt_handling, na.rm = TRUE),

  # IV: Infection
  infection_rate = mean(ab_data$covid_contracted, na.rm = TRUE) * 100,

  # IV: Economic severity
  econ_mean = mean(ab_data$covid_impact_severity, na.rm = TRUE),
  econ_sd = sd(ab_data$covid_impact_severity, na.rm = TRUE),

  # Individual economic impacts
  job_loss_pct = mean(ab_data$covid_job_loss, na.rm = TRUE) * 100,
  income_loss_pct = mean(ab_data$covid_income_loss, na.rm = TRUE) * 100,
  edu_disruption_pct = mean(ab_data$covid_edu_disruption, na.rm = TRUE) * 100,

  # IV: Trust
  trust_mean = mean(ab_data$covid_trust_info, na.rm = TRUE),
  trust_sd = sd(ab_data$covid_trust_info, na.rm = TRUE),

  # Demographics
  age_mean = mean(ab_data$age, na.rm = TRUE),
  age_sd = sd(ab_data$age, na.rm = TRUE),
  female_pct = mean(ab_data$gender == "Female", na.rm = TRUE) * 100,
  urban_pct = mean(ab_data$urban == 1, na.rm = TRUE) * 100,

  # Scale statistics (for Methods section)
  institutional_trust_mean = mean(ab_data$institutional_trust_index, na.rm = TRUE),
  institutional_trust_sd = sd(ab_data$institutional_trust_index, na.rm = TRUE),
  auth_acceptance_mean = mean(ab_data$auth_acceptance, na.rm = TRUE),
  auth_acceptance_sd = sd(ab_data$auth_acceptance, na.rm = TRUE),
  emergency_powers_mean = mean(ab_data$emergency_powers_support, na.rm = TRUE),
  emergency_powers_sd = sd(ab_data$emergency_powers_support, na.rm = TRUE),
  regime_pref_mean = mean(ab_data$regime_preference, na.rm = TRUE),
  regime_pref_sd = sd(ab_data$regime_preference, na.rm = TRUE)
)

# Save to file
saveRDS(
  pooled_descriptives,
  here("papers", "01_vietnam_covid_paradox", "analysis", "results", "descriptive_pooled_sample.rds")
)

cat("✓ Pooled descriptive statistics saved to results/descriptive_pooled_sample.rds\n")
cat("  Contains", length(pooled_descriptives), "statistics\n")
cat("  Sample size:", nrow(ab_data), "\n")
