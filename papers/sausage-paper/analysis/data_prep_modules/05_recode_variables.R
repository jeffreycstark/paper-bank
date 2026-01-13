# ==============================================================================
# 05_recode_variables.R - Recode Variables for Analysis
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================

cat("Module 05: Recoding variables for analysis\n")

library(tidyverse)

# ==============================================================================
# Create binary "sausage mentality" indicator
# ==============================================================================

# The core DV: Do you prioritize economy over democracy?
# dem_vs_econ is on a 1-5 scale where higher values = more economy priority

sausage_recoded <- sausage_composites %>%
  mutate(
    # Rename for clarity
    econ_over_democracy = dem_vs_econ,

    # Binary version: 1 = economy priority (4-5), 0 = democracy priority (1-2)
    # Middle (3) treated as economy priority for conservative test
    sausage_binary = case_when(
      is.na(dem_vs_econ) ~ NA_real_,
      dem_vs_econ >= 3 ~ 1,  # Economy priority (3, 4, 5)
      TRUE ~ 0               # Democracy priority (1, 2)
    ),

    # Standardized version for regression
    econ_priority_z = as.numeric(scale(dem_vs_econ, center = TRUE, scale = TRUE))
  )

# Check distribution
if ("sausage_binary" %in% names(sausage_recoded)) {
  cat("  Sausage binary distribution:\n")
  print(table(sausage_recoded$sausage_binary, useNA = "ifany"))
}

# ==============================================================================
# Recode regime type for regression
# ==============================================================================

sausage_recoded <- sausage_recoded %>%
  mutate(
    # Binary: Democracy vs Non-democracy
    is_democracy = if_else(regime_type == "Democracy", 1, 0),

    # Reference category coding for regression
    regime_factor = relevel(regime_type, ref = "Autocracy")
  )

cat("  Democracy indicator created\n")
cat("  Democracies:", sum(sausage_recoded$is_democracy == 1, na.rm = TRUE), "\n")
cat("  Non-democracies:", sum(sausage_recoded$is_democracy == 0, na.rm = TRUE), "\n")

# ==============================================================================
# Recode demographics for regression
# ==============================================================================

sausage_recoded <- sausage_recoded %>%
  mutate(
    # Gender: female = 1, male = 0
    female = case_when(
      is.na(gender) ~ NA_real_,
      gender == 2 ~ 1,
      gender == 1 ~ 0,
      TRUE ~ NA_real_
    ),

    # Urban: 1 = urban, 0 = rural
    is_urban = case_when(
      is.na(urban_rural) ~ NA_real_,
      urban_rural == 1 ~ 1,
      urban_rural == 2 ~ 0,
      TRUE ~ NA_real_
    ),

    # Age: centered for interaction models
    age_centered = age - mean(age, na.rm = TRUE),
    age_squared = age_centered^2,

    # Education: standardized for regression
    education_z = as.numeric(scale(education_years, center = TRUE, scale = TRUE))
  )

cat("  Demographics recoded\n")

# ==============================================================================
# Create interaction terms for H2
# ==============================================================================

sausage_recoded <- sausage_recoded %>%
  mutate(
    # Education x Regime interaction
    edu_x_democracy = education_z * is_democracy,

    # Quadratic education term for testing inverted-U
    education_z_sq = education_z^2,
    edu_sq_x_democracy = education_z_sq * is_democracy
  )

cat("  Interaction terms created for H2 testing\n")

# ==============================================================================
# Create wave indicators for time trends (H3)
# ==============================================================================

sausage_recoded <- sausage_recoded %>%
  mutate(
    wave_numeric = as.numeric(gsub("w", "", wave)),
    wave_factor = factor(wave, levels = c("w1", "w2", "w3", "w4", "w5", "w6"))
  )

cat("  Wave indicators created\n")

cat("  -> sausage_recoded:", nrow(sausage_recoded), "x", ncol(sausage_recoded), "\n")

# Store for next module
assign("sausage_recoded", sausage_recoded, envir = .GlobalEnv)
