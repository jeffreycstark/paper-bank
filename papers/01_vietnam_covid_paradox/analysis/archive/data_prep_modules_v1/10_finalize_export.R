# ==============================================================================
# MODULE 10: FINALIZE AND EXPORT
# ==============================================================================
# Purpose: Standardize variables, create final datasets, save outputs
# Dependencies: Modules 01-09 (all variables created)
# Inputs: ab_selected (complete dataset)
# Outputs: ab_analysis_v2.rds, ab_analysis_v2_validated.rds, CSV/DTA exports
# ==============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("MODULE 10: FINALIZE AND EXPORT\n")
cat(rep("=", 70), "\n\n")

# ============================================
# STANDARDIZE COMPOSITE INDICES
# ============================================

cat("[finalize] Standardizing composite indices for regression...\n")

ab_analysis <- ab_selected %>%
  mutate(
    institutional_trust_std = standardize_z(institutional_trust_index),
    regime_preference_std = standardize_z(regime_preference),
    auth_acceptance_std = standardize_z(auth_acceptance),
    emergency_powers_std = standardize_z(emergency_powers_support),
    covid_govt_performance_std = standardize_z(covid_govt_performance)
  )

cat("✓ Standardized composites created (*_std variables)\n")

# ============================================
# CREATE VALIDATED DATASET (COMPLETE CASES)
# ============================================

cat("\n[finalize] Creating validated dataset (complete cases only)...\n")

ab_analysis_validated <- ab_analysis %>%
  filter(
    !is.na(institutional_trust_index),
    !is.na(dem_satisfaction_z),
    !is.na(dem_legitimacy_z),
    !is.na(auth_acceptance),
    !is.na(covid_govt_performance)
  )

cat("✓ Validated dataset created\n")
cat("  Full dataset: N =", nrow(ab_analysis), "\n")
cat("  Complete cases: N =", nrow(ab_analysis_validated), "\n")
cat("  Retention rate:", round(100 * nrow(ab_analysis_validated) / nrow(ab_analysis), 1), "%\n")

# ============================================
# SAVE RDS FILES
# ============================================

cat("\n[export] Saving RDS files...\n")

saveRDS(ab_analysis, here("data", "processed", "ab_analysis_v2.rds"))
saveRDS(ab_analysis_validated, here("data", "processed", "ab_analysis_v2_validated.rds"))

cat("✓ Saved: data/processed/ab_analysis_v2.rds\n")
cat("✓ Saved: data/processed/ab_analysis_v2_validated.rds\n")

# ============================================
# OPTIONAL: CSV EXPORT
# ============================================

cat("\n[export] Creating CSV export...\n")

write_csv(ab_analysis, here("data", "processed", "ab_analysis_v2.csv"))

cat("✓ Saved: data/processed/ab_analysis_v2.csv\n")

# ============================================
# FINAL SUMMARY
# ============================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("DATA EXPORT SUMMARY\n")
cat(rep("=", 70), "\n\n")

cat("Full Dataset (ab_analysis_v2):\n")
cat("  - Observations:", nrow(ab_analysis), "\n")
cat("  - Variables:", ncol(ab_analysis), "\n")
cat("  - Countries:", length(unique(ab_analysis$country_name)), "\n")

cat("\nValidated Dataset (ab_analysis_v2_validated):\n")
cat("  - Observations:", nrow(ab_analysis_validated), "\n")
cat("  - Complete case retention:", round(100 * nrow(ab_analysis_validated) / nrow(ab_analysis), 1), "%\n")

cat("\nKey Composites Created:\n")
cat("  - institutional_trust_index\n")
cat("  - dem_satisfaction_z, dem_legitimacy_z\n")
cat("  - regime_preference, auth_acceptance\n")
cat("  - emergency_powers_support\n")
cat("  - covid_govt_performance\n")

cat("\nStandardized Variables (for regression):\n")
cat("  - *_std versions of all composites\n")

cat("\nFiles Saved:\n")
cat("  - RDS: ab_analysis_v2.rds, ab_analysis_v2_validated.rds\n")
cat("  - CSV: ab_analysis_v2.csv\n")

# ============================================
# MODULE COMPLETE
# ============================================

cat("\n✓ MODULE 10 COMPLETE\n")
cat(rep("=", 70), "\n\n")
