# ==============================================================================
# 06_handle_missing.R - Handle Missing Data
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================

cat("Module 06: Handling missing data\n")

library(tidyverse)
library(naniar)  # For missing data visualization

# ==============================================================================
# Document missing data patterns
# ==============================================================================

# Key analysis variables
key_vars <- c(
  "dem_vs_econ",
  "democracy_solves_problems",
  "regime_type",
  "education_z",
  "age",
  "female",
  "is_urban",
  "wave"
)

existing_key <- intersect(key_vars, names(sausage_recoded))

cat("\n  Missing data summary for key variables:\n")
missing_summary <- sausage_recoded %>%
  select(all_of(existing_key)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(
    pct_missing = round(100 * n_missing / nrow(sausage_recoded), 1)
  ) %>%
  arrange(desc(pct_missing))

print(missing_summary)

# ==============================================================================
# Create analysis flags
# ==============================================================================

sausage_missing <- sausage_recoded %>%
  mutate(
    # Flag: has core DV
    has_dv = !is.na(dem_vs_econ),

    # Flag: has all key controls
    has_controls = !is.na(age) & !is.na(female) & !is.na(education_z),

    # Flag: complete case for main analysis
    complete_main = has_dv & has_controls & !is.na(regime_type),

    # Flag: complete case for full model (including all covariates)
    complete_full = complete_main & !is.na(is_urban)
  )

cat("\n  Analysis sample sizes:\n")
cat("  Total observations:", nrow(sausage_missing), "\n")
cat("  Has DV:", sum(sausage_missing$has_dv, na.rm = TRUE), "\n")
cat("  Has controls:", sum(sausage_missing$has_controls, na.rm = TRUE), "\n")
cat("  Complete main:", sum(sausage_missing$complete_main, na.rm = TRUE), "\n")
cat("  Complete full:", sum(sausage_missing$complete_full, na.rm = TRUE), "\n")

# ==============================================================================
# Missing by country (for potential pattern detection)
# ==============================================================================

cat("\n  Missing DV by country:\n")
missing_by_country <- sausage_missing %>%
  group_by(country_name) %>%
  summarise(
    n = n(),
    n_missing_dv = sum(!has_dv),
    pct_missing = round(100 * n_missing_dv / n, 1)
  ) %>%
  arrange(desc(pct_missing))

print(missing_by_country)

# ==============================================================================
# Missing by wave (for potential pattern detection)
# ==============================================================================

cat("\n  Missing DV by wave:\n")
missing_by_wave <- sausage_missing %>%
  group_by(wave) %>%
  summarise(
    n = n(),
    n_missing_dv = sum(!has_dv),
    pct_missing = round(100 * n_missing_dv / n, 1)
  ) %>%
  arrange(wave)

print(missing_by_wave)

# ==============================================================================
# Create listwise deletion dataset for main analysis
# (Keep full dataset for sensitivity analyses)
# ==============================================================================

sausage_complete <- sausage_missing %>%
  filter(complete_main)

cat("\n  Main analysis sample: n =", nrow(sausage_complete), "\n")

cat("  -> sausage_missing (full):", nrow(sausage_missing), "x", ncol(sausage_missing), "\n")
cat("  -> sausage_complete (listwise):", nrow(sausage_complete), "x", ncol(sausage_complete), "\n")

# Store both for next module
assign("sausage_missing", sausage_missing, envir = .GlobalEnv)
assign("sausage_complete", sausage_complete, envir = .GlobalEnv)
