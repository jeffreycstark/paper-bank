# ==============================================================================
# 05_recode_variables.R - Recode Variables for Analysis
# Paper: "I Don't Care How the Sausage Gets Made"
# ==============================================================================
# UPDATED 2026-01-14: Fixed DV coding to be intuitive
#   - econ_over_democracy: Higher = more economy priority (reversed from raw)
#   - Created versions with "both equally" as middle (3) vs as NA
# ==============================================================================

cat("Module 05: Recoding variables for analysis\n")

library(tidyverse)

# ==============================================================================
# CORE DV: Economy vs Democracy Priority ("Sausage Mentality")
# ==============================================================================
# Raw dem_vs_econ coding (from ABS):
#   1 = Economic development is definitely more important
#   2 = Economic development is somewhat more important  
#   3 = Democracy is somewhat more important
#   4 = Democracy is definitely more important
#   5 = They are both equally important (OFF THE CONTINUUM!)
#
# Problem: Higher raw values = more PRO-DEMOCRACY, which is counterintuitive
# for a variable measuring "economy over democracy" preference.
#
# Solution: Reverse the 1-4 scale so higher = more economy priority
# Handle "both equally" (5) in two ways for robustness
# ==============================================================================

sausage_recoded <- sausage_composites %>%
  mutate(
    # -------------------------------------------------------------------------
    # VERSION 1: "Both equally" (5) recoded to middle (2.5 on reversed scale)
    # Use this version for main analysis (preserves all observations)
    # -------------------------------------------------------------------------
    # Step 1: Recode 5 ("both") to 2.5 (middle of 1-4 scale)
    # Step 2: Reverse so higher = economy priority
    #   Raw 1 (econ def) -> 4, Raw 2 (econ somewhat) -> 3
    #   Raw 3 (dem somewhat) -> 2, Raw 4 (dem def) -> 1
    #   Raw 5 (both) -> 2.5 (middle)
    econ_over_democracy = case_when(
      is.na(dem_vs_econ) ~ NA_real_,
      dem_vs_econ == 5 ~ 2.5,                    # "Both equally" -> middle
      dem_vs_econ %in% 1:4 ~ 5 - dem_vs_econ,   # Reverse 1-4 scale
      TRUE ~ NA_real_
    ),
    
    # -------------------------------------------------------------------------
    # VERSION 2: "Both equally" (5) set to NA (strict ordinal interpretation)
    # Use for robustness check - drops ~X% of observations
    # -------------------------------------------------------------------------
    econ_over_democracy_strict = case_when(
      is.na(dem_vs_econ) ~ NA_real_,
      dem_vs_econ == 5 ~ NA_real_,              # "Both equally" -> missing
      dem_vs_econ %in% 1:4 ~ 5 - dem_vs_econ,   # Reverse 1-4 scale
      TRUE ~ NA_real_
    ),
    
    # -------------------------------------------------------------------------
    # Binary version: Clear economy priority (3-4 on new scale) vs not
    # -------------------------------------------------------------------------
    sausage_binary = case_when(
      is.na(econ_over_democracy) ~ NA_real_,
      econ_over_democracy >= 3 ~ 1,   # Economy priority (was raw 1-2)
      econ_over_democracy < 3 ~ 0,    # Democracy priority or neutral
      TRUE ~ NA_real_
    ),
    
    # Standardized version for regression
    econ_priority_z = as.numeric(scale(econ_over_democracy, center = TRUE, scale = TRUE))
  )

# Report on "both equally" recoding
n_both <- sum(sausage_composites$dem_vs_econ == 5, na.rm = TRUE)
n_total <- sum(!is.na(sausage_composites$dem_vs_econ))
cat(sprintf("  'Both equally' responses: %d (%.1f%% of valid)\n", 
            n_both, 100 * n_both / n_total))

# Verify recoding worked correctly
cat("\n  DV Recoding verification:\n")
cat("  Raw dem_vs_econ -> econ_over_democracy (higher = more economy priority)\n")
verification <- sausage_recoded %>%
  filter(!is.na(dem_vs_econ)) %>%
  group_by(dem_vs_econ) %>%
  summarise(
    new_value = first(econ_over_democracy),
    n = n(),
    .groups = "drop"
  )
print(verification)

# ==============================================================================
# SECONDARY DV: Equality vs Freedom (dem_vs_equality)
# ==============================================================================
# Raw coding (from ABS W3-W6):
#   1 = Reducing inequality definitely more important
#   2 = Reducing inequality somewhat more important
#   3 = Political freedom somewhat more important  
#   4 = Political freedom definitely more important
#   5 = Both equally important (OFF THE CONTINUUM!)
#
# Natural coding: Higher = more equality priority
# This matches the question order (inequality mentioned first)
# ==============================================================================

if ("dem_vs_equality" %in% names(sausage_composites)) {
  sausage_recoded <- sausage_recoded %>%
    mutate(
      # VERSION 1: "Both equally" (5) recoded to middle (2.5)
      # Reverse so higher = more equality priority
      equality_over_freedom = case_when(
        is.na(dem_vs_equality) ~ NA_real_,
        dem_vs_equality == 5 ~ 2.5,                      # "Both" -> middle
        dem_vs_equality %in% 1:4 ~ 5 - dem_vs_equality,  # Reverse
        TRUE ~ NA_real_
      ),
      
      # VERSION 2: "Both equally" (5) set to NA
      equality_over_freedom_strict = case_when(
        is.na(dem_vs_equality) ~ NA_real_,
        dem_vs_equality == 5 ~ NA_real_,
        dem_vs_equality %in% 1:4 ~ 5 - dem_vs_equality,
        TRUE ~ NA_real_
      )
    )
  
  n_both_eq <- sum(sausage_composites$dem_vs_equality == 5, na.rm = TRUE)
  n_total_eq <- sum(!is.na(sausage_composites$dem_vs_equality))
  cat(sprintf("\n  Equality vs Freedom 'both equally': %d (%.1f%% of valid)\n",
              n_both_eq, 100 * n_both_eq / n_total_eq))
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

cat("\n  Democracy indicator created\n")
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

# ==============================================================================
# Final summary
# ==============================================================================

cat("\n  === DV CODING SUMMARY ===\n")
cat("  econ_over_democracy: 1-4 scale, higher = more economy priority\n")
cat("    - Raw 1 ('econ definitely') -> 4\n")
cat("    - Raw 2 ('econ somewhat') -> 3\n")
cat("    - Raw 3 ('dem somewhat') -> 2\n")
cat("    - Raw 4 ('dem definitely') -> 1\n")
cat("    - Raw 5 ('both equally') -> 2.5 (middle)\n")
cat("  econ_over_democracy_strict: Same but 'both equally' -> NA\n")
cat("  ========================\n")

cat("\n  -> sausage_recoded:", nrow(sausage_recoded), "x", ncol(sausage_recoded), "\n")

# Store for next module
assign("sausage_recoded", sausage_recoded, envir = .GlobalEnv)
